"""Real process regression: GUI cleanup must leave an untagged sibling alive."""
import importlib.util
import os
from pathlib import Path
import subprocess
import sys
import tempfile

sys.dont_write_bytecode = True
spec = importlib.util.spec_from_file_location('gui_runner', Path(__file__).with_name('run-quarto-gui-check.py'))
runner = importlib.util.module_from_spec(spec)
spec.loader.exec_module(runner)
with tempfile.TemporaryDirectory(prefix='gui-cleanup-', dir='/tmp') as directory:
    root = Path(directory)
    owned = subprocess.Popen(['sleep', '30'], env=dict(os.environ, AWC_GUI_ROOT=str(root)), start_new_session=True)
    sibling = subprocess.Popen(['sleep', '30'], start_new_session=True)
    try:
        assert owned.pid in runner.owned_helpers(root)
        assert sibling.pid not in runner.owned_helpers(root)
        runner.stop_owned_helpers(root)
        owned.wait(timeout=3)
        assert sibling.poll() is None
        assert not runner.owned_helpers(root)
        print('PASS tagged GUI helper reaped; untagged sibling preserved')
    finally:
        for process in (owned, sibling):
            if process.poll() is None:
                process.terminate()
            process.wait(timeout=3)
