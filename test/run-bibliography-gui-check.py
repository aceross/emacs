#!/usr/bin/env python3
"""Check the reference menu/review in a private normal graphical Emacs."""
import argparse
import importlib.util
import json
import os
from pathlib import Path
import shutil
import sys
import tempfile
import time

REPO = Path(__file__).resolve().parent.parent
sys.dont_write_bytecode = True


def helper(name, filename):
    spec = importlib.util.spec_from_file_location(name, REPO / 'test' / filename)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--emacs', default=shutil.which('emacs-xwidgets'))
    args = parser.parse_args()
    if not args.emacs:
        parser.error('Choose an installed graphical Emacs with --emacs')
    runner = helper('reliability', 'run-reliability-checks.py')
    cleanup = helper('graphical', 'run-quarto-gui-check.py')
    root = Path(tempfile.mkdtemp(prefix='awc-bibliography-gui-', dir='/tmp'))
    config = root / 'config'
    config.mkdir()
    for name in ('site-elisp', 'straight', 'tools'):
        (config / name).symlink_to(REPO / name, target_is_directory=True)
    shutil.copyfile(REPO / 'init.el', config / 'init.el')
    first, rest = (REPO / 'early-init.el').read_text().split('\n', 1)
    probe = json.dumps(str(REPO / 'test' / 'reliability-startup-check.el'))
    (config / 'early-init.el').write_text(first + f'\n(load {probe} nil t)\n' + rest)
    env = os.environ.copy()
    for variable in ('PYTHONSTARTUP', 'R_PROFILE_USER', 'R_ENVIRON_USER'):
        env.pop(variable, None)
    env.update(AWC_RELIABILITY_ROOT=str(root),
               AWC_RELIABILITY_RESULT=str(root / 'result.json'),
               AWC_GUI_ROOT=str(root), XDG_CACHE_HOME=str(root / 'cache'),
               GDK_BACKEND='wayland', QUARTO_TEST_RUN_DIR=str(root))
    print(f'Disposable bibliography GUI: {root}', flush=True)
    try:
        runner.run([args.emacs, '--init-directory', str(config), '--no-splash',
                    '--name', 'Disposable reference review', '--load',
                    str(REPO / 'test' / 'bibliography-graphical-session-check.el')],
                   env, root / 'startup.log', timeout=90)
        record = json.loads((root / 'result.json').read_text())
        if record.get('failure') or record.get('warnings'):
            raise RuntimeError(f'Graphical acceptance failed: {record}')
        deadline = time.monotonic() + 5
        while cleanup.owned_helpers(root) and time.monotonic() < deadline:
            time.sleep(0.1)
        if cleanup.owned_helpers(root) or (root / 'server' / 'reliability').exists():
            raise RuntimeError('Graphical helpers/socket survived exit')
        print((root / 'gui-result.txt').read_text(), end='')
        print(f'PASS; screenshot and logs retained: {root}', flush=True)
    finally:
        cleanup.stop_owned_helpers(root)


if __name__ == '__main__':
    main()
