"""A document-owned kernel, controlled by JSON lines; no kernel substitution."""
import ast
import base64
import json
import os
from pathlib import Path
import queue
import re
import signal
import sys
import threading

try:
    from jupyter_client import KernelManager
except ImportError:
    sys.exit("Install jupyter_client and ipykernel explicitly in the selected environment")

artefacts = Path(sys.argv[1])
kernel_name = sys.argv[2]
directory = sys.argv[3]
km = KernelManager(kernel_name=kernel_name)
requests = queue.Queue()
output_lock = threading.Lock()
current = {}


def terminate(_signal, _frame):
    if km.has_kernel:
        km.shutdown_kernel(now=True)
    raise SystemExit(0)


signal.signal(signal.SIGTERM, terminate)


def emit(kind, **values):
    event = {k: current.get(k) for k in ('id', 'document', 'chunk', 'revision')}
    event.update(type=kind, **values)
    with output_lock:
        print('\x1eQUARTO' + json.dumps(event, ensure_ascii=False), flush=True)


def read_requests():
    for line in sys.stdin:
        try:
            request = json.loads(line)
            if request['method'] == 'interrupt':
                km.interrupt_kernel()
            elif request['method'] == 'shutdown':
                km.shutdown_kernel(now=True)
                os._exit(0)
            else:
                requests.put(request)
        except Exception as error:
            emit('error', text=str(error))
    requests.put({'method': 'shutdown'})


def execute(code, expressions=None):
    message_id = client.execute(code, silent=bool(expressions), store_history=not bool(expressions),
                                user_expressions=expressions or {}, allow_stdin=False, stop_on_error=True)
    failed = False
    while True:
        message = client.get_iopub_msg(timeout=None)
        if message.get('parent_header', {}).get('msg_id') != message_id:
            continue
        kind, content = message['msg_type'], message['content']
        if kind == 'status' and content['execution_state'] == 'idle':
            break
        if kind == 'stream':
            emit('warning' if content['name'] == 'stderr' else 'text', text=content['text'])
        elif kind == 'error':
            failed = True
            emit('error', text='\n'.join(content.get('traceback', [content['evalue']])))
        elif kind in ('display_data', 'execute_result'):
            data = content['data']
            if 'image/png' in data:
                path = artefacts / f"plot-{current['id']}-{message['header']['msg_id']}.png"
                path.write_bytes(base64.b64decode(data['image/png']))
                emit('plot', path=str(path))
            elif 'text/plain' in data:
                emit('text', text=data['text/plain'])
    while True:
        reply = client.get_shell_msg(timeout=30)
        if reply.get('parent_header', {}).get('msg_id') == message_id:
            break
    content = reply['content']
    return not failed and content.get('status') == 'ok', content


try:
    # kernel_spec resolution raises when the explicit name does not exist.
    spec = km.kernel_spec
    km.start_kernel(cwd=directory, env=os.environ.copy())
    client = km.client()
    client.start_channels()
    client.wait_for_ready(timeout=45)
    interpreter = None
    if spec.language.lower() == 'python':
        _, identity = execute('', {'interpreter': "__import__('sys').executable"})
        interpreter = ast.literal_eval(identity['user_expressions']['interpreter']['data']['text/plain'])
    emit('ready', engine='jupyter', kernel=kernel_name, argv=spec.argv,
         interpreter=interpreter, language=spec.language, bridge=sys.executable)
    threading.Thread(target=read_requests, daemon=True).start()
    while True:
        current = requests.get()
        if current['method'] == 'shutdown':
            break
        emit('running')
        try:
            if current['language'].lower() != spec.language.lower():
                raise ValueError(f"This kernel runs {spec.language}, not {current['language']}; select a compatible engine")
            if current['method'] == 'table':
                if spec.language.lower() != 'python':
                    raise ValueError('This Jupyter table adapter requires a Python kernel')
                name = current['name']
                if not re.fullmatch(r'[A-Za-z_][A-Za-z0-9_]*', name):
                    raise ValueError('Use a data frame variable name')
                start = max(0, int(current['offset']))
                stop = start + min(200, max(1, int(current['limit'])))
                expression = (
                    f"__import__('json').dumps(dict(columns=list(map(str,{name}.columns)), "
                    f"rows={name}.iloc[{start}:{stop}].astype(str).values.tolist(), "
                    f"total=len({name}), offset={start}))"
                )
                ok, reply = execute('', {'page': expression})
                page = reply.get('user_expressions', {}).get('page', {})
                if page.get('status') != 'ok':
                    raise ValueError(page.get('evalue', 'Table inspection failed'))
                emit('table', **json.loads(ast.literal_eval(page['data']['text/plain'])))
            else:
                ok, reply = execute(current['code'])
            emit('done', status='succeeded' if ok else 'failed')
        except Exception as error:
            emit('error', text=str(error))
            emit('done', status='failed')
except Exception as error:
    emit('fatal', text=str(error))
    raise
finally:
    if km.has_kernel:
        km.shutdown_kernel(now=True)
    if 'client' in globals():
        client.stop_channels()
