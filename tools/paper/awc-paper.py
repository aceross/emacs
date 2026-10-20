#!/usr/bin/env python3
"""Explicit, project-owned manuscript checks and reviewed snapshot manifests.

Only `check --execute` runs configured commands. Status and freeze never do.
All paths in the configuration are relative to its directory. No shell is used.
"""
import argparse
from contextlib import contextmanager
from datetime import datetime, timezone
import fcntl
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import signal
import subprocess
import sys
import tempfile
import time
import uuid

CONFIG = '.awc-paper.json'
STATE = '.awc-paper'
GOOD = {'passed', 'manually-verified', 'not-applicable'}


class PaperError(Exception):
    pass


class Cancelled(Exception):
    pass


def digest(value):
    return hashlib.sha256(json.dumps(value, sort_keys=True, ensure_ascii=True).encode()).hexdigest()


def sha(path):
    with path.open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').hexdigest()


def utc():
    return datetime.now(timezone.utc).isoformat()


def local(root, name):
    """Refuse escaping paths and symlink components, including missing targets."""
    if not isinstance(name, str) or not name or Path(name).is_absolute() or '..' in Path(name).parts:
        raise PaperError(f'Expected a project-relative path: {name!r}')
    path = root / name
    cursor = root
    for part in Path(name).parts:
        cursor = cursor / part
        if cursor.is_symlink():
            raise PaperError(f'Symlinked project paths are unsupported: {name}')
    if not path.resolve().is_relative_to(root):
        raise PaperError(f'Path escapes project: {name}')
    return path


def read(path, default=None):
    if path.is_symlink():
        raise PaperError(f'Refusing symlinked state: {path.name}')
    return json.loads(path.read_text()) if path.exists() else default


def atomic(root, name, value):
    target = local(root, name)
    target.parent.mkdir(parents=True, exist_ok=True, mode=0o700)
    fd, tmp = tempfile.mkstemp(prefix='.pending-', dir=target.parent)
    try:
        with os.fdopen(fd, 'w') as stream:
            json.dump(value, stream, indent=2, ensure_ascii=False)
            stream.write('\n')
            stream.flush()
            os.fsync(stream.fileno())
        os.replace(tmp, target)
    finally:
        if os.path.exists(tmp):
            os.unlink(tmp)


def argv(value):
    if not isinstance(value, list) or not value or not all(isinstance(v, str) and v and '\0' not in v for v in value):
        raise PaperError('Commands must be nonempty argv arrays, never shell strings')
    return value


def load_config(path):
    root = path.parent.resolve()
    local(root, path.name)
    cfg = read(path)
    if not isinstance(cfg, dict) or cfg.get('version') != 1:
        raise PaperError('A project .awc-paper.json with version 1 is required')
    if set(cfg) - {'version', 'inputs', 'checks', 'manual', 'tools'}:
        raise PaperError('Unknown top-level configuration key')
    if not isinstance(cfg.get('inputs'), list) or not cfg['inputs']:
        raise PaperError('Declare the source/configuration/rule/script inputs explicitly')
    for pattern in cfg['inputs']:
        local(root, pattern)
    ids = set()
    for key in ('checks', 'manual'):
        if not isinstance(cfg.get(key, []), list) or not all(isinstance(v, dict) for v in cfg.get(key, [])):
            raise PaperError(f'{key} must be an array of objects')
    if not isinstance(cfg.get('tools', {}), dict):
        raise PaperError('tools must map names to version-command arrays')
    for check in cfg.get('checks', []) + cfg.get('manual', []):
        name = check.get('id', '')
        if not re.fullmatch(r'[a-z][a-z0-9-]*', name) or name in ids:
            raise PaperError('Check/manual IDs must be unique lowercase names')
        ids.add(name)
    for check in cfg.get('checks', []):
        if set(check) - {'id', 'type', 'argv', 'success', 'outputs', 'reject', 'timeout', 'env', 'files', 'limit', 'policy', 'skip'}:
            raise PaperError(f'Unknown check option: {check["id"]}')
        argv(check.get('argv'))
        kind = check.get('type', 'command')
        if kind not in {'command', 'word-count'}:
            raise PaperError(f'Unsupported check type: {kind}')
        if not 0 < check.get('timeout', 60) <= 3600:
            raise PaperError('A check timeout must be in (0, 3600] seconds')
        if not isinstance(check.get('env', {}), dict) or not all(isinstance(k, str) and isinstance(v, str) for k, v in check.get('env', {}).items()):
            raise PaperError('env must map names to strings')
        for key in ('outputs', 'files'):
            if key not in check:
                continue
            if not isinstance(check[key], list):
                raise PaperError(f'{key} must be an array of project-relative file paths')
            names = [local(root, name).relative_to(root).as_posix() for name in check[key]]
            if any(name == '.' or Path(name).parts[0] in {STATE, '.git'} for name in names):
                raise PaperError('Outputs/documents must be files outside internal state')
            check[key] = names
        if kind == 'command' and check.get('success') not in {'exit-code', 'fresh-outputs'}:
            raise PaperError('Each command needs an explicit success contract')
        if check.get('success') == 'fresh-outputs' and not check.get('outputs'):
            raise PaperError('fresh-outputs requires declared outputs')
        if kind == 'word-count':
            if not check.get('files'):
                raise PaperError('word-count requires source files')
            if 'limit' in check and (not isinstance(check['limit'], int) or check['limit'] < 1 or not check.get('policy', {}).get('source') or not check.get('policy', {}).get('date')):
                raise PaperError('A word limit requires a positive limit and policy source/date')
        for pattern in check.get('reject', []):
            re.compile(pattern)
    for manual in cfg.get('manual', []):
        if set(manual) - {'id', 'description', 'source', 'date', 'not_applicable'}:
            raise PaperError('Unknown manual requirement option')
        if not manual.get('description'):
            raise PaperError('Manual requirements need a description')
        if manual.get('source') and not manual.get('date'):
            raise PaperError('Sourced requirements need their review date')
    for command in cfg.get('tools', {}).values():
        argv(command)
    return root, cfg


def executable(root, command, extra_env=None):
    search = (extra_env or {}).get('PATH', os.environ.get('PATH', ''))
    # Relative command paths and relative PATH components resolve in the project.
    search = os.pathsep.join(str(root / p) if not Path(p).is_absolute() else p for p in search.split(os.pathsep))
    name = command[0]
    found = str(root / name) if '/' in name and not Path(name).is_absolute() else shutil.which(name, path=search)
    if not found or not Path(found).is_file() or not os.access(found, os.X_OK):
        return {'command': name, 'available': False}
    path = Path(found).resolve()
    return {'command': name, 'available': True, 'path': str(path), 'sha256': sha(path)}


def git_provenance(root, files):
    """Read HEAD/index only; never refresh a worktree or run hooks/filters.

    Dirty input markers compare raw blob bytes and executable bits, without
    Git's clean filters, text conversion, or assume-unchanged shortcuts.
    """
    command = ['git', '--no-lazy-fetch', '--no-optional-locks', '--literal-pathspecs',
               '-c', 'core.fsmonitor=false', '-c', 'core.hooksPath=/dev/null']
    def git(*args, check=True):
        return subprocess.run(command + list(args), cwd=root, capture_output=True,
                              text=True, timeout=5, check=check)
    try:
        revision = git('rev-parse', '--show-object-format', 'HEAD', check=False)
        if revision.returncode:
            if revision.returncode != 128:
                revision.check_returncode()
            return {'head': None, 'status': 'unversioned'}
        algorithm, head = revision.stdout.splitlines()
        if algorithm not in {'sha1', 'sha256'}:
            raise PaperError(f'Unsupported Git object format: {algorithm}')
        names = sorted(files)
        index, conflicts, tree = {}, set(), {}
        for entry in git('ls-files', '--stage', '-z', '--', *names).stdout.split('\0')[:-1]:
            meta, name = entry.split('\t', 1)
            mode, oid, stage = meta.split()
            if stage != '0':
                conflicts.add(name)
            else:
                index[name] = (mode, oid)
        for entry in git('ls-tree', '-r', '-z', 'HEAD', '--', *names).stdout.split('\0')[:-1]:
            meta, name = entry.split('\t', 1)
            mode, _kind, oid = meta.split()
            tree[name] = (mode, oid)
        dirty = []
        for name in names:
            if name in conflicts:
                dirty.append('UU ' + name)
                continue
            if name not in index:
                if name in tree:
                    dirty.append('D  ' + name)
                dirty.append('?? ' + name)
                continue
            path = local(root, name)
            stat = path.stat()
            blob = hashlib.new(algorithm, f'blob {stat.st_size}\0'.encode())
            with path.open('rb') as stream:
                while chunk := stream.read(1024 * 1024):
                    blob.update(chunk)
            worktree = ('100755' if stat.st_mode & 0o111 else '100644', blob.hexdigest())
            staged = 'A' if name not in tree else ('M' if index[name] != tree[name] else ' ')
            unstaged = 'M' if worktree != index[name] else ' '
            if staged + unstaged != '  ':
                dirty.append(staged + unstaged + ' ' + name)
        return {'head': head, 'input_status': dirty, 'comparison': 'raw-bytes-and-executable-bit'}
    except (OSError, ValueError, subprocess.SubprocessError) as error:
        return {'head': None, 'status': 'unavailable', 'detail': str(error)}


def snapshot(root, config_path, cfg):
    files = {config_path.name: sha(config_path)}
    missing = []
    for pattern in cfg['inputs']:
        matches = sorted(p for p in root.glob(pattern) if p.is_file() or p.is_symlink())
        if not matches:
            missing.append(pattern)
        for file in matches:
            name = file.relative_to(root).as_posix()
            local(root, name)
            if Path(name).parts[0] in {STATE, '.git'}:
                raise PaperError('Input patterns must exclude .git and .awc-paper state')
            files[name] = sha(file)
    outputs = {name for check in cfg.get('checks', []) for name in check.get('outputs', [])}
    if outputs & files.keys():
        raise PaperError('Generated outputs must not overlap input patterns')
    for check in cfg.get('checks', []):
        if set(check.get('files', [])) - files.keys():
            raise PaperError('Word-count sources must also be declared inputs')
    tools = {check['id']: executable(root, check['argv'], check.get('env')) for check in cfg.get('checks', [])}
    tools.update({'version:' + name: executable(root, cmd) for name, cmd in cfg.get('tools', {}).items()})
    git = git_provenance(root, files)
    value = {'files': files, 'missing': missing, 'git': git, 'executables': tools, 'runner_sha256': sha(Path(__file__).resolve())}
    value['id'] = digest(value)
    return value


def artifact(root, name, metadata=False):
    path = local(root, name)
    if not path.is_file():
        return None
    value = {'sha256': sha(path), 'bytes': path.stat().st_size}
    if metadata:
        stat = path.stat()
        value.update(mtime_ns=stat.st_mtime_ns, ctime_ns=stat.st_ctime_ns)
    return value


def word_count(ast):
    """Count body/headings/notes/link labels; omit metadata/code/math/citations."""
    excluded = {'CodeBlock', 'Code', 'Math', 'Cite', 'RawBlock', 'RawInline', 'Image'}
    def count(node):
        if isinstance(node, list):
            return sum(map(count, node))
        if not isinstance(node, dict) or node.get('t') in excluded:
            return 0
        if node.get('t') == 'Str':
            return len(re.findall(r"\b\w+(?:['’\-]\w+)*\b", node['c']))
        return count(node.get('c', []))
    return count(ast['blocks'])


@contextmanager
def locked(root):
    path = local(root, STATE + '/lock')
    path.parent.mkdir(mode=0o700, exist_ok=True)
    with path.open('a+') as stream:
        try:
            fcntl.flock(stream, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError as error:
            raise PaperError('Another manuscript action is running in this project') from error
        try:
            yield
        finally:
            fcntl.flock(stream, fcntl.LOCK_UN)


def busy(root):
    path = local(root, STATE + '/lock')
    if not path.exists():
        return False
    with path.open('r') as stream:
        try:
            fcntl.flock(stream, fcntl.LOCK_EX | fcntl.LOCK_NB)
            fcntl.flock(stream, fcntl.LOCK_UN)
            return False
        except BlockingIOError:
            return True


def owner_start(pid):
    try:
        return Path(f'/proc/{pid}/stat').read_text().rsplit(')', 1)[1].split()[19]
    except (OSError, IndexError):
        return None


def owner_alive(record):
    pid = record.get('owner_pid')
    if not isinstance(pid, int) or pid < 1:
        return False
    try:
        os.kill(pid, 0)
    except OSError:
        return False
    return not record.get('owner_start') or owner_start(pid) == record['owner_start']


def execution_plan(cfg):
    """Expand the exact argv once in the runner, for review and execution."""
    checks = []
    for spec in cfg.get('checks', []):
        if spec.get('skip'):
            commands = []
        elif spec.get('type') == 'word-count':
            commands = [spec['argv'] + ['--from=markdown', '--to=json', source] for source in spec['files']]
        else:
            commands = [spec['argv']]
        checks.append(dict(spec, commands=commands))
    return dict(cfg, checks=checks)


def status(root, config_path, cfg):
    current = snapshot(root, config_path, cfg)
    run = read(local(root, STATE + '/latest.json'), {})
    manuals = read(local(root, STATE + '/manual.json'), {})
    outputs = {name: artifact(root, name) for check in cfg.get('checks', []) for name in check.get('outputs', [])}
    context = digest({'snapshot': current['id'], 'outputs': outputs})
    stale = bool(run and run.get('snapshot', {}).get('id') != current['id'])
    checks = []
    by_id = {item['id']: item for item in run.get('checks', [])}
    running = busy(root) and owner_alive(run)
    for spec in cfg.get('checks', []):
        item = dict(by_id.get(spec['id'], {'id': spec['id'], 'state': 'not-run'}))
        if stale:
            item.update(state='stale', detail='Inputs, Git revision, runner or executable identity changed')
        elif item['state'] == 'running' and not running:
            item.update(state='cancelled', detail='The checking process was interrupted')
        elif item['state'] == 'passed' and any(item.get('outputs', {}).get(name) != outputs[name] for name in spec.get('outputs', [])):
            item.update(state='stale', detail='A checked output changed or disappeared')
        if item.get('log_sha256') and artifact(root, item['log']) != item['log_sha256']:
            item.update(state='stale', detail='The validation log changed or disappeared')
        checks.append(item)
    manual = []
    for spec in cfg.get('manual', []):
        entry = dict(spec, state='not-run')
        if spec.get('not_applicable'):
            entry.update(state='not-applicable', detail=spec['not_applicable'])
        elif spec['id'] in manuals:
            entry.update(manuals[spec['id']])
            if entry.get('context') != context:
                entry.update(state='stale', detail='Review no longer covers these inputs/outputs')
        manual.append(entry)
    versions_ok = all(tool.get('exit') == 0 and tool.get('version', '').strip() for tool in run.get('tools', {}).values())
    ready = bool(checks) and not current['missing'] and versions_ok and all(c['state'] in GOOD for c in checks + manual)
    plan = execution_plan(cfg)
    value = {'version': 1, 'snapshot': current, 'run': run, 'checks': checks, 'manual': manual,
             'outputs': outputs, 'ready': ready, 'state': 'ready' if ready else 'incomplete',
             'plan': plan, 'review_token': digest({'snapshot': current, 'run': run, 'manual': manuals, 'outputs': outputs, 'checks': checks, 'plan': plan})}
    return value


def run_command(root, command, log, timeout, env=None):
    """Stop only this command's process group on timeout, cancellation or exit."""
    child = None
    with log.open('wb') as stream:
        try:
            child = subprocess.Popen(command, cwd=root, env={**os.environ, **(env or {})}, stdout=stream,
                                     stderr=subprocess.STDOUT, start_new_session=True)
            try:
                return child.wait(timeout=timeout)
            except subprocess.TimeoutExpired:
                raise PaperError(f'Command timed out after {timeout}s')
        finally:
            if child is not None:
                # Also reap helpers left by an already exited parent command.
                for sig in (signal.SIGTERM, signal.SIGKILL):
                    try:
                        os.killpg(child.pid, sig)
                    except ProcessLookupError:
                        break
                    try:
                        child.wait(timeout=0.3)
                    except subprocess.TimeoutExpired:
                        pass
                child.wait()


def check_project(root, config_path, cfg):
    start = snapshot(root, config_path, cfg)
    if start['missing']:
        raise PaperError('Missing declared inputs: ' + ', '.join(start['missing']))
    name = uuid.uuid4().hex
    directory = local(root, f'{STATE}/runs/{name}')
    directory.mkdir(parents=True, mode=0o700)
    record = {'id': name, 'owner_pid': os.getpid(), 'owner_start': owner_start(os.getpid()), 'started': utc(), 'snapshot': start, 'tools': {},
              'checks': [dict(id=s['id'], state='not-run') for s in cfg.get('checks', [])]}
    cancelled = False
    def save():
        atomic(root, STATE + '/latest.json', record)
    save()
    try:
        for tool, command in cfg.get('tools', {}).items():
            log = directory / ('version-' + re.sub(r'[^a-zA-Z0-9-]', '_', tool) + '.log')
            try:
                code = run_command(root, command, log, 5)
                record['tools'][tool] = {'argv': command, 'exit': code, 'version': log.read_text(errors='replace')[:4000]}
            except (OSError, PaperError) as error:
                record['tools'][tool] = {'argv': command, 'unavailable': str(error)}
        for spec, item in zip(execution_plan(cfg)['checks'], record['checks']):
            if snapshot(root, config_path, cfg)['id'] != start['id']:
                item.update(state='stale', detail='Inputs changed during validation; rerun')
                break
            if spec.get('skip'):
                item.update(state='skipped', detail=spec['skip'])
                save()
                continue
            log = directory / (spec['id'] + '.log')
            item.update(state='running', argv=spec['argv'], log=log.relative_to(root).as_posix())
            save()
            before = {n: artifact(root, n, True) for n in spec.get('outputs', [])}
            began = time.monotonic()
            try:
                item['commands'] = []
                if spec.get('type') == 'word-count':
                    counts = {}
                    for source, command in zip(spec['files'], spec['commands']):
                        item['commands'].append(command)
                        code = run_command(root, command, log, spec.get('timeout', 60), spec.get('env'))
                        if code:
                            raise PaperError(f'Pandoc exited {code}; inspect log')
                        counts[source] = word_count(json.loads(log.read_text()))
                    item.update(counts=counts, total=sum(counts.values()), state='passed')
                    if 'limit' in spec and item['total'] > spec['limit']:
                        item.update(state='failed', detail=f'Word count {item["total"]} exceeds configured limit {spec["limit"]}')
                else:
                    item['commands'].append(spec['commands'][0])
                    code = run_command(root, spec['commands'][0], log, spec.get('timeout', 60), spec.get('env'))
                    item.update(exit=code, state='passed' if code == 0 else 'failed')
                    text = log.read_text(errors='replace')
                    rejected = [pattern for pattern in spec.get('reject', []) if re.search(pattern, text)]
                    if rejected:
                        item.update(state='failed', detail='Configured failure diagnostic matched', rejected=rejected)
                    if item['state'] == 'passed' and spec['success'] == 'fresh-outputs':
                        for output in spec['outputs']:
                            after = artifact(root, output, True)
                            if not after or not after['bytes'] or after == before[output]:
                                item.update(state='failed', detail=f'Expected a fresh, nonempty output: {output}')
                item['outputs'] = {n: artifact(root, n) for n in spec.get('outputs', [])}
            except FileNotFoundError as error:
                item.update(state='unavailable', detail=str(error))
            except (OSError, PaperError, ValueError, KeyError) as error:
                item.update(state='failed', detail=str(error))
            item['seconds'] = round(time.monotonic() - began, 3)
            item['log_sha256'] = artifact(root, item['log'])
            save()
    except Cancelled:
        cancelled = True
        for item in record['checks']:
            if item['state'] == 'running':
                item.update(state='cancelled', detail='Cancelled explicitly')
    finally:
        record['finished'] = utc()
        record['cancelled'] = cancelled
        save()
    return status(root, config_path, cfg)


def cancel_signal(_signal, _frame):
    raise Cancelled()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--config', type=Path, default=Path(CONFIG))
    parser.add_argument('--expect', help='Require the exact reviewed status token before mutating')
    sub = parser.add_subparsers(dest='action', required=True)
    sub.add_parser('status')
    check = sub.add_parser('check')
    check.add_argument('--execute', action='store_true', help='Explicitly run the displayed project commands')
    verify = sub.add_parser('verify')
    verify.add_argument('id')
    verify.add_argument('--reviewer', required=True)
    verify.add_argument('--note', required=True)
    freeze = sub.add_parser('freeze')
    freeze.add_argument('--label', required=True)
    args = parser.parse_args()
    try:
        path = args.config.absolute()
        root, cfg = load_config(path)
        if args.action == 'status':
            value = status(root, path, cfg)
        else:
            with locked(root):
                value = status(root, path, cfg)
                if args.expect and args.expect != value['review_token']:
                    raise PaperError('The reviewed project state changed; inspect status again')
                if args.action == 'check':
                    if not args.execute:
                        raise PaperError('Review status.plan, then use check --execute to run project commands')
                    signal.signal(signal.SIGTERM, cancel_signal)
                    signal.signal(signal.SIGINT, cancel_signal)
                    value = check_project(root, path, cfg)
                elif args.action == 'verify':
                    if not args.reviewer.strip() or not args.note.strip():
                        raise PaperError('Manual review requires a reviewer and a concrete note')
                    if args.id not in {m['id'] for m in cfg.get('manual', []) if not m.get('not_applicable')}:
                        raise PaperError('Unknown or not-applicable manual requirement')
                    if value['snapshot']['missing']:
                        raise PaperError('Cannot verify a snapshot with missing inputs')
                    entries = read(local(root, STATE + '/manual.json'), {})
                    entries[args.id] = dict(state='manually-verified', reviewer=args.reviewer, note=args.note,
                                           reviewed=utc(), context=digest({'snapshot': value['snapshot']['id'], 'outputs': value['outputs']}))
                    atomic(root, STATE + '/manual.json', entries)
                    value = status(root, path, cfg)
                else:
                    if not args.label.strip():
                        raise PaperError('A reviewed checkpoint needs a label')
                    checkpoint = f'{STATE}/checkpoints/{uuid.uuid4().hex}.json'
                    atomic(root, checkpoint, dict(value, label=args.label, frozen=utc()))
                    value['checkpoint'] = checkpoint
        print(json.dumps(value, ensure_ascii=False))
        return 1 if args.action == 'check' and not value['ready'] else 0
    except (PaperError, OSError, ValueError, TypeError, KeyError, subprocess.SubprocessError) as error:
        print(json.dumps({'error': str(error)}))
        return 2


if __name__ == '__main__':
    sys.exit(main())
