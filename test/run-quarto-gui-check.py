#!/usr/bin/env python3
"""Run a separate graphical Emacs against an existing disposable integration run."""
import argparse
import json
import os
from pathlib import Path
import shutil
import signal
import subprocess
import tempfile
import time

REPO = Path(__file__).resolve().parent.parent


def owned_helpers(gui):
    """Find surviving Linux helpers by the exact inherited fixture tag."""
    tag = ('AWC_GUI_ROOT=' + str(gui)).encode()
    found = []
    for process in Path('/proc').iterdir():
        if not process.name.isdigit():
            continue
        try:
            if process.stat().st_uid != os.getuid():
                continue
            if tag in (process / 'environ').read_bytes().split(b'\0'):
                if (process / 'stat').read_text().rsplit(')', 1)[1].split()[0] != 'Z':
                    found.append(int(process.name))
        except (FileNotFoundError, PermissionError, ProcessLookupError):
            pass
    return found


def stop_owned_helpers(gui):
    """Reap only this attempt's tagged helpers, including detached kernels."""
    for sig in (signal.SIGTERM, signal.SIGKILL):
        for pid in owned_helpers(gui):
            try:
                os.kill(pid, sig)
            except ProcessLookupError:
                pass
        deadline = time.monotonic() + 2
        while owned_helpers(gui) and time.monotonic() < deadline:
            time.sleep(0.1)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--fixture-root', required=True, type=Path)
    parser.add_argument('--seconds', type=int, default=900, help='Active soak duration after functional checks')
    parser.add_argument('--interval', type=int, default=60, help='Seconds between active cycles (shorter only for harness smoke checks)')
    parser.add_argument('--emacs', default=shutil.which('emacs-xwidgets'))
    args = parser.parse_args()
    root = args.fixture_root.resolve()
    if not root.is_relative_to('/tmp') or not (root / 'environment.json').is_file():
        parser.error('Choose a retained /tmp/quarto-checks-* integration fixture')
    if not args.emacs or args.seconds < 60 or args.interval < 10:
        parser.error('Choose the installed graphical Emacs and a duration of at least 60 seconds')
    # Every GUI attempt gets new state; dependencies/projects remain in the
    # selected disposable run. Do not resume an older GUI process or server.
    gui = Path(tempfile.mkdtemp(prefix='gui-', dir=root))
    config = gui / 'config'
    config.mkdir()
    for name in ('site-elisp', 'straight', 'tools'):
        (config / name).symlink_to(REPO / name, target_is_directory=True)
    shutil.copyfile(REPO / 'init.el', config / 'init.el')
    first, rest = (REPO / 'early-init.el').read_text().split('\n', 1)
    setup = json.dumps(str(REPO / 'test' / 'quarto-graphical-setup.el'))
    (config / 'early-init.el').write_text(first + f'\n(load {setup} nil t)\n' + rest)
    env = os.environ.copy()
    for variable in ('PYTHONSTARTUP', 'R_PROFILE_USER', 'R_ENVIRON_USER'):
        env.pop(variable, None)
    env.update(json.loads((root / 'environment.json').read_text()))
    env.update(AWC_GUI_ROOT=str(gui), AWC_GUI_SECONDS=str(args.seconds),
               AWC_GUI_INTERVAL=str(args.interval), GDK_BACKEND='wayland')
    command = [args.emacs, '--init-directory', str(config), '--no-splash',
               '--name', 'Disposable Emacs validation',
               '--load', str(REPO / 'test' / 'quarto-graphical-session-check.el')]
    print(f'Graphical validation: {gui}; soak: {args.seconds}s', flush=True)
    process = None
    try:
        with (gui / 'startup.log').open('wb') as log:
            process = subprocess.Popen(command, cwd=REPO, env=env, stdout=log,
                                       stderr=subprocess.STDOUT, start_new_session=True)
            (gui / 'emacs.pid').write_text(str(process.pid) + '\n')
            deadline = time.monotonic() + args.seconds + 360
            while process.poll() is None:
                if time.monotonic() > deadline:
                    raise RuntimeError('Graphical validation exceeded its outer deadline')
                # The GUI owns asynchronous timers. Polling its process does
                # not block Emacs's command loop or synthesize a passing result.
                time.sleep(1)
        result = gui / 'result.json'
        if process.returncode or not result.is_file():
            raise RuntimeError(f'Graphical Emacs exited {process.returncode}; inspect startup.log/result.log')
        record = json.loads(result.read_text())
        if record.get('status') != 'passed':
            raise RuntimeError(f'Graphical check failed: {record}')
        deadline = time.monotonic() + 10
        while owned_helpers(gui) and time.monotonic() < deadline:
            time.sleep(0.2)
        if owned_helpers(gui):
            raise RuntimeError(f'Graphical helpers survived exit: {owned_helpers(gui)}')
        if (gui / 'server' / 'disposable-gui').exists():
            raise RuntimeError('The dedicated graphical socket survived exit')
        print(json.dumps(record, indent=2), flush=True)
        print(f'PASS GUI; retained evidence: {gui}', flush=True)
        return 0
    except (OSError, RuntimeError, ValueError) as error:
        print(f'{error}\nEvidence retained at {gui}', flush=True)
        return 1
    finally:
        if process is not None and process.poll() is None:
            os.killpg(process.pid, signal.SIGTERM)
            try:
                process.wait(timeout=3)
            except subprocess.TimeoutExpired:
                os.killpg(process.pid, signal.SIGKILL)
                process.wait()
        stop_owned_helpers(gui)


if __name__ == '__main__':
    raise SystemExit(main())
