#!/usr/bin/env python3
"""Run Quarto checks in fresh disposable projects; never install by default."""
import argparse
import json
import os
from pathlib import Path
import shutil
import signal
import subprocess
import sys
import tempfile

REPO = Path(__file__).resolve().parent.parent


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--suite', choices=('core', 'assistance', 'runtime', 'output', 'all'), default='core')
    parser.add_argument('--emacs', default=shutil.which('emacs-xwidgets') or shutil.which('emacs'))
    parser.add_argument('--python', default=sys.executable, help='Python used to create disposable virtual environments')
    parser.add_argument('--r-library', action='append', default=[], help='Additional existing R dependency library')
    parser.add_argument('--install-dependencies', action='store_true', help='Explicitly allow missing R/Python dependencies to be installed into the disposable test directory')
    parser.add_argument('--keep', action='store_true', help='Retain fixtures and logs after successful checks')
    args = parser.parse_args()
    if not args.emacs:
        parser.error('Choose an installed Emacs with --emacs')
    root = Path(tempfile.mkdtemp(prefix='quarto-checks-', dir='/tmp'))
    logs = root / 'logs'
    logs.mkdir()
    env = os.environ.copy()
    # Project fixture profiles must win over inherited user startup settings.
    for variable in ('PYTHONSTARTUP', 'R_PROFILE_USER', 'R_ENVIRON_USER'):
        env.pop(variable, None)
    env['QUARTO_TEST_RUN_DIR'] = str(root)
    env['AWC_R_LSP_TEST_DIR'] = str(root / 'lsp')
    env['QUARTO_TEST_RUNTIME_ROOT'] = str(root / 'runtime' / 'project with spaces')
    env['QUARTO_TEST_PYTHON'] = args.python
    env['PIP_CACHE_DIR'] = str(root / 'pip-cache')
    env['MPLCONFIGDIR'] = str(root / 'matplotlib')
    env['XDG_CACHE_HOME'] = str(root / 'cache')
    env['RENV_PATHS_ROOT'] = str(root / 'renv-cache')
    env['RENV_CONFIG_SANDBOX_ENABLED'] = 'FALSE'
    env['RENV_CONFIG_CONSENT'] = 'TRUE'
    env['JUPYTER_CONFIG_DIR'] = str(root / 'jupyter-config')
    env['JUPYTER_RUNTIME_DIR'] = str(root / 'jupyter-runtime')
    env['JUPYTER_DATA_DIR'] = str(root / 'jupyter-data')
    # Retain enough information to replay an integration stage in this exact
    # disposable environment after a failure, without repeating installations.
    def save_environment():
        keys = ('QUARTO_TEST_RUN_DIR', 'AWC_R_LSP_TEST_DIR', 'QUARTO_TEST_RUNTIME_ROOT',
                'QUARTO_TEST_PYTHON', 'PIP_CACHE_DIR', 'MPLCONFIGDIR', 'XDG_CACHE_HOME',
                'R_LIBS_USER', 'RENV_PATHS_ROOT', 'RENV_CONFIG_SANDBOX_ENABLED',
                'RENV_CONFIG_CONSENT', 'JUPYTER_CONFIG_DIR', 'JUPYTER_RUNTIME_DIR',
                'JUPYTER_DATA_DIR', 'JUPYTER_PATH')
        (root / 'environment.json').write_text(json.dumps({k: env[k] for k in keys if k in env}, indent=2) + '\n')
    library = root / 'lsp' / 'R-library'
    library.mkdir(parents=True)
    success = False

    def run(name, command, timeout=240):
        save_environment()
        print(f'Running {name}', flush=True)
        path = logs / f'{name}.log'
        with path.open('w') as output:
            process = subprocess.Popen(command, cwd=REPO, env=env, stdout=output,
                                       stderr=subprocess.STDOUT, start_new_session=True)
            try:
                result = process.wait(timeout=timeout)
            except subprocess.TimeoutExpired:
                # Reap this test's helpers as well as Emacs after a deadline.
                os.killpg(process.pid, signal.SIGTERM)
                try:
                    process.wait(timeout=3)
                except subprocess.TimeoutExpired:
                    os.killpg(process.pid, signal.SIGKILL)
                    process.wait()
                raise
        if result:
            diagnostic = path.read_text(errors='replace')
            print(diagnostic if len(diagnostic) < 5000 else diagnostic[:3000] + '\n…\n' + diagnostic[-1500:], file=sys.stderr)
            raise RuntimeError(f'{name} failed; see {path}')

    def elisp(name, file):
        run(name, [args.emacs, '-Q', '--batch', '-l', str(REPO / 'test' / file)])

    def r_dependencies(packages):
        normal_libraries = subprocess.check_output(
            ['Rscript', '--vanilla', '-e', 'cat(paste(.libPaths(), collapse=.Platform$path.sep))'],
            env=os.environ, text=True, timeout=20).strip()
        env['R_LIBS_USER'] = os.pathsep.join([str(library), *args.r_library, normal_libraries])
        # R receives names as arguments, never interpolated executable code.
        script = root / 'dependencies.R'
        script.write_text('''args <- commandArgs(TRUE)
pkgs <- args[-1]
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly=TRUE)]
if (length(missing) && args[1] == "install") {
  install.packages(missing, lib=Sys.getenv("R_LIBS_USER") |> strsplit(.Platform$path.sep) |> unlist() |> head(1), repos="https://cloud.r-project.org")
}
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly=TRUE)]
if (length(missing)) stop("Missing R dependencies: ", paste(missing, collapse=", "), ". Supply --r-library or explicitly use --install-dependencies.")
''')
        run(f'r-dependencies-{packages[0]}', ['Rscript', '--vanilla', str(script),
                               'install' if args.install_dependencies else 'check', *packages], timeout=600)

    try:
        elisp('regression', 'run-tests.el')
        elisp('safety-startup', 'safety-startup-check.el')
        preferences = root / 'preferences-startup'
        env['AWC_SAFETY_TEST_ROOT'] = str(preferences)
        try:
            for stage in ('save', 'load', 'corrupt'):
                env['AWC_PREFERENCES_CHECK_STAGE'] = stage
                if stage == 'corrupt':
                    custom = preferences / 'config' / 'var' / 'preferences' / 'custom.el'
                    marker = preferences / 'preferences-ran'
                    custom.write_text(';;; -*- lexical-binding: t -*-\n'
                                      f'(write-region "ran" nil "{marker}")\n(custom-set-variables\n')
                elisp(f'preferences-{stage}', 'preferences-startup-check.el')
        finally:
            env.pop('AWC_SAFETY_TEST_ROOT', None)
            env.pop('AWC_PREFERENCES_CHECK_STAGE', None)
        elisp('ess-startup', 'ess-startup-check.el')
        elisp('prose-startup', 'prose-startup-check.el')
        elisp('yaml-schemas', 'quarto-yaml-integration.el')
        elisp('research-assistance', 'quarto-research-integration.el')
        elisp('literature-workflow', 'awc-research-quarto-integration.el')
        if args.suite in ('assistance', 'all'):
            r_dependencies(['renv', 'languageserver'])
            for label in ('alpha', 'beta'):
                run(f'fixture-{label}', ['Rscript', '--vanilla', 'test/r-lsp-fixtures.R', label])
            elisp('language-assistance', 'quarto-assistance-integration.el')
            elisp('r-language-assistance', 'r-lsp-integration.el')
        if args.suite in ('runtime', 'output', 'all'):
            r_dependencies(['jsonlite', 'evaluate', 'knitr', 'reticulate', 'rmarkdown'])
            run('r-versions', ['Rscript', '--vanilla', '-e',
                              'pkgs <- intersect(c("renv", "languageserver", "jsonlite", "evaluate", "knitr", "reticulate", "rmarkdown", "httpgd"), rownames(installed.packages())); print(installed.packages()[pkgs, c("Package", "Version", "LibPath")]); sessionInfo()'])
            project = Path(env['QUARTO_TEST_RUNTIME_ROOT'])
            (project / 'reports' / 'nested space').mkdir(parents=True)
            (project / 'fixture.Rproj').write_text('Version: 1.0\n')
            (project / '_quarto.yml').write_text('project:\n  type: default\nformat:\n  html: default\n  pdf: default\n')
            (project / '.Rprofile').write_text('options(quarto.test.startup = getwd())\n')
            (project / '.Renviron').write_text('# Disposable test environment\n')
            run('python-environment', [args.python, '-m', 'venv', str(project / '.venv')])
            python = str(project / '.venv' / 'bin' / 'python')
            packages = ['jupyter_client', 'ipykernel', 'pandas', 'matplotlib', 'nbclient', 'nbformat', 'PyYAML']
            if args.install_dependencies:
                run('python-dependencies', [python, '-m', 'pip', 'install', *packages], timeout=600)
            run('python-preflight', [python, '-c',
                                    'import jupyter_client, ipykernel, pandas, matplotlib, nbclient, nbformat, yaml'])
            env['JUPYTER_CONFIG_DIR'] = str(root / 'jupyter-config')
            env['JUPYTER_RUNTIME_DIR'] = str(root / 'jupyter-runtime')
            env['JUPYTER_PATH'] = str(project / '.venv' / 'share' / 'jupyter')
            run('python-kernel', [python, '-m', 'ipykernel', 'install', '--sys-prefix', '--name', 'quarto-fixture'])
            run('python-versions', [python, '-m', 'pip', 'freeze'])
            if args.suite in ('runtime', 'all'):
                elisp('document-runtimes', 'quarto-runtime-integration.el')
                elisp('document-lifecycle', 'quarto-lifecycle-integration.el')
            if args.suite in ('output', 'all'):
                elisp('render-and-preview', 'quarto-output-integration.el')
        success = True
        print(f'PASS {args.suite}; logs: {logs}', flush=True)
    except (OSError, RuntimeError, subprocess.TimeoutExpired) as error:
        print(f'{error}\nFixtures and logs retained at {root}', file=sys.stderr)
        return 1
    finally:
        if success and not args.keep:
            shutil.rmtree(root)
            print('Disposable fixtures removed; use --keep to retain logs.')
    return 0


if __name__ == '__main__':
    sys.exit(main())
