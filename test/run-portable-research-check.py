#!/usr/bin/env python3
"""Exercise two private Emacs profiles against disposable read-only Zotero APIs."""
import argparse
from contextlib import contextmanager
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
import json
import os
from pathlib import Path
import shutil
import signal
import subprocess
import tempfile
import threading
from urllib.parse import urlsplit

REPO = Path(__file__).resolve().parent.parent


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--emacs', default=shutil.which('emacs-xwidgets') or shutil.which('emacs'))
    parser.add_argument('--gui', action='store_true', help='Also open the received PDF in a private normal GUI')
    args = parser.parse_args()
    root = Path(tempfile.mkdtemp(prefix='awc-portable-research-', dir='/tmp'))
    requests = []
    env = os.environ.copy()
    for key in ('PYTHONSTARTUP', 'R_PROFILE_USER', 'R_ENVIRON_USER'):
        env.pop(key, None)
    env.update(HOME=str(root / 'empty-home'), XDG_CACHE_HOME=str(root / 'cache'),
               XDG_DATA_HOME=str(root / 'local-data'), QUARTO_TEST_RUN_DIR=str(root))
    Path(env['HOME']).mkdir()
    print(f'Disposable portable research: {root}', flush=True)

    def run(label, command, extra=None, cwd=REPO):
        with (root / f'{label}.log').open('w') as log:
            process = subprocess.Popen(command, cwd=cwd, env=env | (extra or {}), stdout=log,
                                       stderr=subprocess.STDOUT, start_new_session=True)
            try:
                result = process.wait(timeout=120)
            except subprocess.TimeoutExpired:
                os.killpg(process.pid, signal.SIGTERM)
                try:
                    process.wait(timeout=3)
                except subprocess.TimeoutExpired:
                    os.killpg(process.pid, signal.SIGKILL)
                    process.wait()
                raise
        if result:
            raise RuntimeError(f'{label} failed: {(root / f"{label}.log").read_text()[-6000:]}')

    devices = [root / 'device alpha', root / 'device beta']
    for index, device in enumerate(devices):
        (device / 'vault').mkdir(parents=True)
        (device / 'storage').mkdir()
        tex = device / 'storage' / 'source.tex'
        tex.write_text(r'\documentclass{article}\begin{document}' +
                       f'Device {index + 1}: portable reference attachment.' +
                       r'\end{document}' + '\n')
        run(f'pdf-{index}', ['pdflatex', '-interaction=nonstopmode', '-halt-on-error',
                             '-output-directory', str(tex.parent), str(tex)])
        shutil.copyfile(tex.with_suffix('.pdf'), tex.parent / 'Paper é #1 ?.pdf')
        shutil.copyfile(tex.with_suffix('.pdf'), tex.parent / 'Appendix.pdf')
    alpha, beta = devices
    (alpha / 'existing.bib').write_text(
        '@article{legacy:key,title={A source},author={Person, Alice},year={2024},'
        'doi={10.1/source},file={' + str(alpha / 'storage' / 'Paper é #1 ?.pdf') + '}}\n')

    @contextmanager
    def api(device, native):
        class Handler(BaseHTTPRequestHandler):
            def log_message(self, *_):
                pass

            def do_GET(self):
                requests.append({'device': device.name, 'method': 'GET', 'path': self.path})
                path = urlsplit(self.path).path
                prefix = '/api/users/12345/'
                metadata = {'type': 'user', 'id': 12345}
                response, content_type, status = None, 'application/json', 200
                if path in ('/api/users/0/items/ABC12345', prefix + 'items/ABC12345'):
                    response = {'key': 'ABC12345', 'library': metadata,
                                'data': {'title': 'A source', 'citationKey': native}}
                elif path == prefix + 'items':
                    content_type = 'text/plain'
                    response = ('@article{' + native + ',title={A source},author={Person, Alice},'
                                'year={2024},doi={10.1/source},file={' +
                                str(device / 'storage' / 'Paper é #1 ?.pdf') + '}}\n')
                elif path == prefix + 'items/ABC12345/children':
                    response = [dict(key=key, library=metadata,
                                     data=dict(itemType='attachment', parentItem='ABC12345',
                                               contentType='application/pdf', title=key, deleted=0))
                                for key in ('PDF12345', 'PDF67890', 'MISSING1')]
                elif path.startswith(prefix + 'items/') and path.endswith('/file/view/url'):
                    attachment = path.split('/')[-4]
                    name = {'PDF12345': 'Paper é #1 ?.pdf', 'PDF67890': 'Appendix.pdf'}.get(attachment)
                    content_type = 'text/plain'
                    if name:
                        response = (device / 'storage' / name).as_uri()
                    else:
                        response, status = 'Not downloaded', 404
                else:
                    response, status = {'error': 'wrong library or route'}, 400
                body = (json.dumps(response) if content_type == 'application/json' else response).encode()
                self.send_response(status)
                self.send_header('Content-Type', content_type + '; charset=utf-8')
                self.send_header('Content-Length', str(len(body)))
                self.end_headers()
                self.wfile.write(body)

        server = ThreadingHTTPServer(('127.0.0.1', 0), Handler)
        thread = threading.Thread(target=server.serve_forever, daemon=True)
        thread.start()
        try:
            yield f'http://127.0.0.1:{server.server_port}/api/'
        finally:
            server.shutdown()
            server.server_close()
            thread.join()
            (root / 'requests.json').write_text(json.dumps(requests, indent=2) + '\n')

    def stage(name, device, base=None):
        extra = dict(AWC_PORTABLE_DEVICE=str(device), AWC_PORTABLE_STAGE=name,
                     AWC_PORTABLE_API=base or 'http://127.0.0.1:1/api/')
        run(name, [args.emacs, '-Q', '--batch', '-l',
                   str(REPO / 'test' / 'portable-research-session-check.el')], extra)

    with api(alpha, 'native-original') as base:
        stage('send', alpha, base)
    packet = json.loads((root / 'transfer.json').read_text())
    assert packet['associations'][0]['citekey'] == 'legacy:key'
    assert packet['associations'][0]['library'] == 'users/12345'
    assert str(root) not in (root / 'transfer.json').read_text()
    shutil.copyfile(alpha / 'existing.bib', beta / 'existing.bib')
    shutil.copytree(alpha / 'vault', beta / 'vault', dirs_exist_ok=True)
    shutil.copytree(alpha / 'manuscript', beta / 'manuscript')
    note = beta / 'vault' / 'My renamed reading note.md'
    note_before = note.read_bytes()
    stage('receive', beta)  # No server exists while receiving or opening.
    with api(beta, 'native-changed') as base:
        stage('resolve', beta, base)
    stage('offline', beta)
    assert note.read_bytes() == note_before
    assert (alpha / 'existing.bib').read_bytes() == (beta / 'existing.bib').read_bytes()
    cache = (beta / 'data' / 'zotero-files-local.json').read_text()
    assert str(beta) in cache and str(alpha) not in cache
    for output in ('html', 'pdf'):
        run(f'offline-render-{output}', ['quarto', 'render', str(beta / 'manuscript' / 'paper.qmd'), '--to', output], cwd=root)
    html = (beta / 'manuscript' / 'paper.html').read_text()
    assert 'id="ref-legacy:key"' in html and 'data-cites="legacy:key"' in html
    assert 'citation not found' not in html and '<<CITATION>>' not in html
    run('pdf-text', ['pdftotext', str(beta / 'manuscript' / 'paper.pdf'), str(root / 'manuscript.txt')])
    assert 'Person' in (root / 'manuscript.txt').read_text()
    if args.gui:
        run('graphical', ['python3', str(REPO / 'test' / 'run-bibliography-gui-check.py'),
                          '--emacs', args.emacs, '--check', 'portable'],
            dict(AWC_PORTABLE_DEVICE=str(beta)))
    (root / 'requests.json').write_text(json.dumps(requests, indent=2) + '\n')
    print(f'PASS: independent profiles; actual GET transport; canonical key and renamed note; '
          f'local PDFs; offline HTML/PDF. Evidence: {root}', flush=True)


if __name__ == '__main__':
    main()
