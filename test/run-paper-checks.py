#!/usr/bin/env python3
"""Real P3 acceptance in disposable Quarto projects; no dependency installs."""
import argparse
import importlib.util
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import time

REPO = Path(__file__).resolve().parent.parent
RUNNER = REPO / 'tools/paper/awc-paper.py'


def fixture(root):
    project = root / 'manuscript with spaces é'
    project.mkdir()
    (project / 'scripts').mkdir()
    cfg = json.loads((REPO / 'etc/paper/.awc-paper.json').read_text())
    cfg['inputs'] += ['rules/*.txt']
    (project / 'rules').mkdir()
    (project / 'rules/terms.txt').write_text('Synthetic terminology policy.\n')
    (project / '.awc-paper.json').write_text(json.dumps(cfg, indent=2) + '\n')
    (project / '_quarto.yml').write_text('project:\n  type: default\nformat:\n  html:\n    embed-resources: true\n  pdf:\n    pdf-engine: pdflatex\n')
    (project / 'references.bib').write_text('@article{fixture2024, title={Synthetic reference}, author={Person, Alex}, year={2024}, journal={Fixture Journal}}\n')
    (project / 'paper.qmd').write_text('''---
title: Fixture manuscript
engine: markdown
bibliography: references.bib
---
# Findings

We analyse three outcomes with care [@fixture2024].

The measured association requires independent replication.

`ignored_code` $x=ignored_math$.

```python
ignored_code = 42
```
''')
    (project / 'supplement.qmd').write_text('---\ntitle: Fixture supplement\nengine: markdown\n---\n# Additional evidence\n\nThe supplement describes sampling choices.\n')
    (project / 'requirements.md').write_text('# Synthetic fixture policy\n\nDated 2026-10-19. This is not a real journal policy.\nReview the manuscript and supplement before recording manual verification.\n')
    (project / 'scripts/check_outputs.py').write_text('''from pathlib import Path
import sys
html = Path('paper.html').read_text()
if 'id="ref-fixture2024"' not in html or 'data-cites="fixture2024"' not in html:
    print('paper.qmd:8:1: error: Expected rendered citation/reference is missing')
    sys.exit(1)
assert Path('paper.pdf').read_bytes().startswith(b'%PDF')
assert 'Additional evidence' in Path('supplement.html').read_text()
print('PASS: rendered citation, PDF signature, and supplement')
''')
    return project


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--emacs', default=shutil.which('emacs-xwidgets'))
    parser.add_argument('--gui', action='store_true')
    parser.add_argument('--keep', action='store_true')
    args = parser.parse_args()
    root = Path(tempfile.mkdtemp(prefix='emacs-paper-qa-', dir='/tmp'))
    project = fixture(root)
    env = os.environ.copy()
    env.update(AWC_PAPER_QA_ROOT=str(root), AWC_PAPER_QA_PROJECT=str(project),
               XDG_CONFIG_HOME=str(root / 'xdg-config'), XDG_CACHE_HOME=str(root / 'cache'),
               HOME=str(root / 'home'), PYTHONDONTWRITEBYTECODE='1')
    (root / 'home').mkdir()
    print(f'Manuscript QA: {root}', flush=True)
    success = False

    def call(*args, code=0, where=project):
        result = subprocess.run([sys.executable, str(RUNNER), '--config', str(where / '.awc-paper.json'), *args],
                                env=env, capture_output=True, text=True, timeout=180)
        assert result.returncode == code, result.stdout + result.stderr
        return json.loads(result.stdout)

    def run(name, command, timeout=180):
        with (root / f'{name}.log').open('w') as stream:
            subprocess.run(command, cwd=REPO, env=env, stdout=stream, stderr=subprocess.STDOUT,
                           check=True, timeout=timeout)

    try:
        run('unit', [sys.executable, str(REPO / 'test/test_paper_runner.py')])
        start = time.monotonic()
        initial = call('status')
        assert initial['state'] == 'incomplete' and not (project / '.awc-paper').exists()
        assert initial['plan']['checks'][-1]['commands'] == [
            ['quarto', 'pandoc', '--from=markdown', '--to=json', name]
            for name in ('paper.qmd', 'supplement.qmd')]
        result = call('check', '--execute', code=1)  # Manual review is still pending.
        (root / 'cli-check.json').write_text(json.dumps(result, indent=2) + '\n')
        assert all(item['state'] == 'passed' for item in result['checks']), result['checks']
        assert [item['commands'] for item in result['checks']] == [spec['commands'] for spec in initial['plan']['checks']]
        assert result['checks'][-1]['total'] == 20, result['checks'][-1]
        ready = call('verify', 'submission-policy', '--reviewer', 'Synthetic reviewer', '--note',
                     'Inspected both fixture artifacts and the dated synthetic policy')
        assert ready['ready']
        manifest = call('--expect', ready['review_token'], 'freeze', '--label', 'CLI fixture checkpoint')
        checkpoint = project / manifest['checkpoint']
        preserved = checkpoint.read_bytes()
        assert json.loads(preserved)['ready']
        # A moved project retains path-relative artifact provenance on this host.
        copied = root / 'independent copy é'
        shutil.copytree(project, copied)
        assert call('status', where=copied)['ready']
        # Rule and reference changes invalidate the exact prior checks/review.
        for name in ('rules/terms.txt', 'references.bib', '_quarto.yml', 'scripts/check_outputs.py'):
            path = project / name
            before = path.read_bytes()
            path.write_bytes(before + b'\n')
            changed = call('status')
            assert all(item['state'] == 'stale' for item in changed['checks'])
            assert changed['manual'][0]['state'] == 'stale'
            path.write_bytes(before)
        # Real Quarto can exit successfully with an unresolved citation; the
        # configured warning contract and rendered-output check must both fail.
        paper = project / 'paper.qmd'
        original = paper.read_text()
        paper.write_text(original.replace('@fixture2024', '@missingfixture'))
        broken = call('check', '--execute', code=1)
        assert broken['checks'][0]['state'] == 'failed' and broken['checks'][3]['state'] == 'failed'
        (root / 'broken-citation.json').write_text(json.dumps(broken, indent=2) + '\n')
        failed_freeze = call('freeze', '--label', 'Unresolved citation reviewed')
        assert not json.loads((project / failed_freeze['checkpoint']).read_text())['ready']
        assert checkpoint.read_bytes() == preserved
        paper.write_text(original)
        # Restore real current artifacts for Emacs parity and graphical review.
        call('check', '--execute', code=1)
        call('verify', 'submission-policy', '--reviewer', 'Synthetic reviewer', '--note', 'Rechecked restored artifacts')
        stable = call('status')
        (root / 'expected-status.json').write_text(json.dumps(stable) + '\n')
        run('emacs', [args.emacs, '-Q', '--batch', '-l', str(REPO / 'test/awc-paper-integration.el')])
        if args.gui:
            spec = importlib.util.spec_from_file_location('gui_helpers', REPO / 'test/run-quarto-gui-check.py')
            helpers = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(helpers)
            png_spec = importlib.util.spec_from_file_location('png_helpers', REPO / 'test/run-writing-checks.py')
            png = importlib.util.module_from_spec(png_spec)
            png_spec.loader.exec_module(png)
            gui = root / 'gui'
            config = gui / 'config'
            config.mkdir(parents=True)
            for name in ('site-elisp', 'straight', 'tools'):
                (config / name).symlink_to(REPO / name, target_is_directory=True)
            shutil.copyfile(REPO / 'init.el', config / 'init.el')
            first, rest = (REPO / 'early-init.el').read_text().split('\n', 1)
            setup = json.dumps(str(REPO / 'test/quarto-graphical-setup.el'))
            (config / 'early-init.el').write_text(first + f'\n(load {setup} nil t)\n' + rest)
            env.update(AWC_GUI_ROOT=str(gui), GDK_BACKEND='wayland')
            try:
                run('gui-startup', [args.emacs, '--init-directory', str(config), '--no-splash',
                                    '--name', 'Disposable manuscript QA', '--load', str(REPO / 'test/awc-paper-gui-check.el')])
                gui_result = json.loads((gui / 'result.json').read_text())
                assert gui_result['status'] == 'passed', gui_result
                for name in ('status.png', 'stale.png', 'review-cancelled.png', 'plan.png'):
                    png.validate_png(gui / name)
                assert not helpers.owned_helpers(gui), 'Owned GUI helpers survived'
                assert not (gui / 'server/disposable-gui').exists(), 'Private socket survived'
            finally:
                helpers.stop_owned_helpers(gui)
        elapsed = time.monotonic() - start
        (root / 'result.json').write_text(json.dumps({'status': 'passed', 'words': 20, 'checks': 5,
                                                     'seconds': elapsed, 'gui': args.gui}, indent=2) + '\n')
        success = True
        print(f'PASS manuscript acceptance ({elapsed:.2f}s); evidence: {root}', flush=True)
    finally:
        if success and not args.keep:
            shutil.rmtree(root)
        elif not success:
            print(f'FAILED; evidence retained: {root}', flush=True)


if __name__ == '__main__':
    main()
