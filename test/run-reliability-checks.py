#!/usr/bin/env python3
"""Measure installed Emacs startup and exercise disposable project lifecycles.

Uses existing packages/tools read-only. Never installs dependencies. Terminal
launches use a private PTY, normal early-init/init and normal startup hooks.
"""
import argparse
import fcntl
import json
import os
from pathlib import Path
import pty
import select
import shutil
import signal
import statistics
import struct
import subprocess
import tempfile
import termios
import time

REPO = Path(__file__).resolve().parent.parent


def claim_terminal():
    """Make the child's PTY stdin its controlling terminal after setsid."""
    fcntl.ioctl(0, termios.TIOCSCTTY, 0)


def run(command, env, log, *, terminal=False, timeout=180):
    master = slave = None
    if terminal:
        master, slave = pty.openpty()
        fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack('HHHH', 42, 155, 0, 0))
    started = time.monotonic()
    with log.open('wb') as output:
        try:
            process = subprocess.Popen(
                command, cwd=REPO, env=env, start_new_session=True,
                preexec_fn=claim_terminal if terminal else None,
                stdin=slave if terminal else subprocess.DEVNULL,
                stdout=slave if terminal else output,
                stderr=slave if terminal else subprocess.STDOUT)
            if slave is not None:
                os.close(slave)
                slave = None
            while process.poll() is None:
                if time.monotonic() - started > timeout:
                    raise subprocess.TimeoutExpired(command, timeout)
                if terminal and select.select([master], [], [], 0.1)[0]:
                    try:
                        output.write(os.read(master, 65536))
                        output.flush()
                    except OSError:
                        pass  # PTY reports EIO when the child closes it.
                elif not terminal:
                    try:
                        process.wait(timeout=0.1)
                    except subprocess.TimeoutExpired:
                        pass
            if terminal:
                while select.select([master], [], [], 0)[0]:
                    try:
                        data = os.read(master, 65536)
                        if not data:
                            break
                        output.write(data)
                    except OSError:
                        break
            if process.returncode:
                raise RuntimeError(f'Exit {process.returncode}; see {log}')
        finally:
            if 'process' in locals() and process.poll() is None:
                os.killpg(process.pid, signal.SIGTERM)
                try:
                    process.wait(timeout=3)
                except subprocess.TimeoutExpired:
                    os.killpg(process.pid, signal.SIGKILL)
                    process.wait()
            for descriptor in (master, slave):
                if descriptor is not None:
                    os.close(descriptor)
    return round(time.monotonic() - started, 3)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--suite', choices=('startup', 'session', 'all'), default='all')
    parser.add_argument('--emacs', default=shutil.which('emacs-xwidgets') or shutil.which('emacs'))
    parser.add_argument('--python', default=shutil.which('python3'))
    parser.add_argument('--repetitions', type=int, default=5)
    parser.add_argument('--cycles', type=int, default=3)
    parser.add_argument('--keep', action='store_true')
    args = parser.parse_args()
    if not args.emacs or args.repetitions < 2 or args.cycles < 2:
        parser.error('An installed Emacs, at least two launches and two cycles are required')
    root = Path(tempfile.mkdtemp(prefix='emacs-reliability-', dir='/tmp'))
    logs = root / 'logs'
    logs.mkdir()
    env = os.environ.copy()
    # Let each fixture's own .Rprofile run; never run an inherited Python
    # startup script or a caller-selected R profile in an integration check.
    for variable in ('PYTHONSTARTUP', 'R_PROFILE_USER'):
        env.pop(variable, None)
    environ = root / 'empty.Renviron'
    environ.touch()
    env.update(AWC_RELIABILITY_ROOT=str(root), QUARTO_TEST_RUN_DIR=str(root),
               TERM='xterm-256color', MPLCONFIGDIR=str(root / 'matplotlib'),
               XDG_CACHE_HOME=str(root / 'cache'), R_ENVIRON_USER=str(environ))
    success = False
    try:
        if args.suite in ('startup', 'all'):
            config = root / 'config'
            config.mkdir()
            for name in ('site-elisp', 'straight'):
                (config / name).symlink_to(REPO / name, target_is_directory=True)
            shutil.copyfile(REPO / 'init.el', config / 'init.el')
            probe = json.dumps(str(REPO / 'test' / 'reliability-startup-check.el'))
            first, rest = (REPO / 'early-init.el').read_text().split('\n', 1)
            (config / 'early-init.el').write_text(
                first + f'\n(load {probe} nil t)\n' + rest)
            command = [args.emacs, '-nw', '--init-directory', str(config), '--no-splash',
                       '--eval', "(add-hook 'emacs-startup-hook #'awc-reliability-finish t)"]
            results = []
            for index in range(args.repetitions):
                print(f'Normal terminal startup {index + 1}/{args.repetitions}', flush=True)
                result = logs / f'startup-{index + 1}.json'
                env['AWC_RELIABILITY_RESULT'] = str(result)
                elapsed = run(command, env, logs / f'startup-{index + 1}.log', terminal=True)
                record = json.loads(result.read_text())
                record['wall_seconds'] = elapsed
                results.append(record)
            # Keep essential commands (notably Git) while hiding optional tools.
            hidden = {'quarto', 'node', 'nodejs', 'R', 'Rscript', 'python', 'python3',
                      'pyright', 'pyright-langserver'}
            tools = root / 'without-optional-tools'
            tools.mkdir()
            for directory in os.get_exec_path(env):
                path = Path(directory)
                if path.is_dir():
                    for tool in path.iterdir():
                        target = tools / tool.name
                        if tool.name not in hidden and not target.is_symlink() and not target.exists():
                            if tool.is_file() and os.access(tool, os.X_OK):
                                target.symlink_to(tool.absolute())
            missing_env = dict(env, PATH=str(tools), AWC_RELIABILITY_MISSING='1',
                               AWC_RELIABILITY_ORIGINAL_PATH=env['PATH'],
                               AWC_RELIABILITY_RESULT=str(logs / 'missing-tools.json'))
            print('Normal startup and editing without optional executables', flush=True)
            run(command, missing_env, logs / 'missing-tools.log', terminal=True)
            summary = {'launches': results,
                       'median_init_seconds': statistics.median(x['init_seconds'] for x in results),
                       'median_wall_seconds': statistics.median(x['wall_seconds'] for x in results)}
            (logs / 'startup-summary.json').write_text(json.dumps(summary, indent=2) + '\n')
            print(json.dumps(summary, indent=2), flush=True)
        if args.suite in ('session', 'all'):
            for tool in ('R', 'Rscript', 'quarto', 'node'):
                if not shutil.which(tool):
                    raise RuntimeError(f'Session suite needs existing {tool}; nothing was installed')
            if not args.python:
                raise RuntimeError('Session suite needs an existing Python for disposable venvs')
            for label in ('project alpha', 'project β'):
                project = root / label
                (project / 'reports' / 'nested space').mkdir(parents=True)
                (project / 'fixture.Rproj').write_text('Version: 1.0\n')
                (project / '_quarto.yml').write_text('project:\n  type: default\nformat: html\n')
                (project / '.Rprofile').write_text(f'options(awc.fixture = "{label}")\n')
                run([args.python, '-m', 'venv', '--without-pip', str(project / '.venv')],
                    env, logs / f'venv-{label}.log')
            env['AWC_RELIABILITY_CYCLES'] = str(args.cycles)
            print(f'Two-project lifecycle, {args.cycles} cycles', flush=True)
            run([args.emacs, '-Q', '--batch', '-l', str(REPO / 'test' / 'reliability-session-check.el')],
                env, logs / 'session.log', timeout=360)
        success = True
        print(f'PASS {args.suite}; evidence: {logs}', flush=True)
        return 0
    except (OSError, RuntimeError, subprocess.TimeoutExpired, ValueError) as error:
        print(f'{error}\nFixtures and logs retained at {root}', flush=True)
        return 1
    finally:
        if success and not args.keep:
            shutil.rmtree(root)
            print('Disposable fixtures removed; use --keep to retain evidence.')


if __name__ == '__main__':
    raise SystemExit(main())
