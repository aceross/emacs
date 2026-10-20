#!/usr/bin/env python3
"""Evaluate project prose QA in disposable synthetic projects; no installs."""
import argparse
from collections import Counter
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import time
import zlib

REPO = Path(__file__).resolve().parent.parent


def validate_png(path):
    """Reject corrupted binary frame exports before claiming capture success."""
    data = path.read_bytes()
    assert data[:8] == b'\x89PNG\r\n\x1a\n', path
    offset = 8
    compressed = bytearray()
    while offset < len(data):
        size = int.from_bytes(data[offset:offset + 4])
        chunk = data[offset + 4:offset + 8 + size]
        checksum = int.from_bytes(data[offset + 8 + size:offset + 12 + size])
        assert zlib.crc32(chunk) == checksum, (path, offset)
        if chunk[:4] == b'IDAT':
            compressed.extend(chunk[4:])
        offset += size + 12
    assert offset == len(data) and zlib.decompress(compressed), path


def fixtures(root, dictionaries):
    project = root / 'project with spaces é'
    shutil.copytree(REPO / 'etc' / 'writing', project)
    target = project / 'styles' / 'config' / 'dictionaries'
    target.mkdir()
    for suffix in ('aff', 'dic'):
        shutil.copyfile(dictionaries / f'en_GB.{suffix}', target / f'en_GB.{suffix}')
    # A deliberately synthetic domain term demonstrates project dictionaries.
    with (project / 'styles/config/ignore/project.txt').open('a') as stream:
        stream.write('neuroflux\n')
    expected = {}
    for extension in ('md', 'qmd', 'org'):
        org = extension == 'org'
        lines = (['#+TITLE: Results', '* Methods'] if org else
                 ['---', 'title: "TODO data-set recieved"', 'author: "XYZ"', '---', '# Methods'])
        findings = []

        def add(text, *matches):
            lines.append(text)
            for rule, match in matches:
                start = text.index(match) + 1
                findings.append((len(lines), start, start + len(match) - 1, f'Academic.{rule}', match))

        add('Élise 😀\tuses the data-set in order to compare results. TODO recieved.',
            ('Terms', 'data-set'), ('Wordiness', 'in order to'),
            ('Placeholders', 'TODO'), ('Spelling', 'recieved'))
        add('The color of the sample changes.', ('Spelling', 'color'))
        add('The XYZ estimate needs a definition.', ('Acronyms', 'XYZ'))
        add('ABC precedes its definition.', ('Acronyms', 'ABC'))
        add('The Academic Board Committee (ABC) meets today. ABC is now defined.')
        add('The National Health Service (NHS) supports this work.')
        add('NHS is defined in the preceding paragraph. DNA is an intentional exception.')
        add('We analyse behaviour, colour and the organisation of the neuroflux dataset.')
        add('The samples were collected at noon. The estimate differs significantly from zero.')
        add('Several participants have few observations, as specified by the sampling design.')
        add('The following examples are syntax, not prose errors.')
        add('~TODO data-set recieved~' if org else '`TODO data-set recieved`')
        add('[cite:@TODO; @recieved]' if org else '[see @TODO, p. 3; @recieved]')
        if org:
            add('[cite/text:@TODO]')
        else:
            add('The narrative citation @TODO is a reference key.')
        add('$TODO = data-set + recieved$')
        add(r'\(TODO = data-set\)')
        add('$$')
        add('TODO = data-set + recieved')
        add('$$')
        add(r'\[')
        add('TODO = data-set + recieved')
        add(r'\]')
        add('[[https://example.org/TODO/recieved]]' if org else
            '[Source](https://example.org/TODO/recieved)')
        add('The identifier recieved_value names a column.')
        add('#+begin_src python' if org else '```{python}')
        add('#| label: TODO')
        add('# TODO data-set recieved XYZ in order to')
        add('value = "TODO"')
        add('#+end_src' if org else '```')
        if org:
            add('#+begin_example')
            add('TODO data-set recieved')
            add('#+end_example')
            add('#+BeGiN_SrC python')
            add('value = "TODO data-set recieved $$ equation $$"')
            add('#+EnD_SrC')
            add('# vale Academic.Placeholders = NO')
            add('TODO')
            add('# vale Academic.Placeholders = YES')
        else:
            add('<!-- vale Academic.Placeholders = NO -->')
            add('TODO')
            add('<!-- vale Academic.Placeholders = YES -->')
        if extension == 'qmd':
            add('::: {.callout-note #TODO}')
            add('The prose in a callout remains checked. TBD', ('Placeholders', 'TBD'))
            add(':::')
        add('The next data-set needs revision. TODO recieved.',
            ('Terms', 'data-set'), ('Placeholders', 'TODO'), ('Spelling', 'recieved'))
        add('')
        add(' '.join(['The model estimates an association between the observed outcome and '
                      'the recorded exposure. We report uncertainty and describe the sample '
                      'selection procedure. These results require replication in another population.'] * 20))
        name = f'draft.{extension}'
        (project / name).write_text('\n'.join(lines) + '\n')
        expected[name] = findings
    (project / 'chapters').mkdir()
    (project / 'chapters/relaxed.md').write_text('We work in order to compare results. TODO neuroflux\n')
    with (project / '.vale.ini').open('a') as stream:
        stream.write('\n[chapters/relaxed.md]\nAcademic.Wordiness = NO\n')
    # Substantial but controlled document: length measures runtime, not scholarly skill.
    paragraph = ('The model estimates an association between the observed outcome and the '
                 'recorded exposure. We report uncertainty and describe the sample selection '
                 'procedure. These results require replication in another population.\n\n')
    (project / 'long.qmd').write_text('# Substantial synthetic manuscript\n\n' + paragraph * 500 + 'TODO\n')
    (root / 'expected.json').write_text(json.dumps(expected, indent=2) + '\n')
    return project, expected


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--vale', default=shutil.which('vale'), required=not shutil.which('vale'))
    parser.add_argument('--emacs', default=shutil.which('emacs-xwidgets'))
    parser.add_argument('--dictionaries', type=Path, default=Path('/usr/share/hunspell'))
    parser.add_argument('--gui', action='store_true', help='Private normal Wayland session with real keyboard dispatch')
    parser.add_argument('--keep', action='store_true')
    args = parser.parse_args()
    root = Path(tempfile.mkdtemp(prefix='emacs-writing-qa-', dir='/tmp'))
    print(f'Writing QA fixtures: {root}', flush=True)
    success = False
    env = os.environ.copy()
    env.update(AWC_WRITING_ROOT=str(root), AWC_WRITING_VALE=str(Path(args.vale).resolve()),
               XDG_CONFIG_HOME=str(root / 'xdg-config'), XDG_CACHE_HOME=str(root / 'cache'))
    env.pop('VALE_CONFIG_PATH', None)
    env.pop('VALE_STYLES_PATH', None)

    def run(name, command, timeout=90):
        with (root / f'{name}.log').open('w') as log:
            subprocess.run(command, cwd=REPO, env=env, stdout=log,
                           stderr=subprocess.STDOUT, check=True, timeout=timeout)

    try:
        project, expected = fixtures(root, args.dictionaries)
        command = [args.vale, '--no-global', '--no-exit', '--output=JSON',
                   '--config=' + str(project / '.vale.ini')]
        start = time.monotonic()
        result = subprocess.run(command + list(expected), cwd=project, env=env,
                                check=True, capture_output=True, text=True, timeout=30)
        elapsed = time.monotonic() - start
        (root / 'corpus.json').write_text(result.stdout)
        observed = json.loads(result.stdout)
        assert not result.stderr.strip(), result.stderr
        useful = false_positives = missing = words = lines = 0
        missed_cases = []
        per_rule = Counter()
        for name, targets in expected.items():
            content = (project / name).read_text()
            words += len(content.split())
            lines += len(content.splitlines())
            actual = []
            for finding in observed.get(name, []):
                line, (first, last) = finding['Line'], finding['Span']
                match = content.splitlines()[line - 1][first - 1:last]
                assert match == finding['Match'], (name, finding, match)
                actual.append((line, first, last, finding['Check'], finding['Match']))
                per_rule[finding['Check']] += 1
            actual, targets = Counter(actual), Counter(map(tuple, targets))
            useful += sum((actual & targets).values())
            false_positives += sum((actual - targets).values())
            missing += sum((targets - actual).values())
            missed_cases.extend((name, *case) for case in (targets - actual).elements())
        start = time.monotonic()
        long_result = subprocess.run(command + ['long.qmd'], cwd=project, env=env,
                                     check=True, capture_output=True, text=True, timeout=30)
        long_seconds = time.monotonic() - start
        (root / 'long.json').write_text(long_result.stdout)
        long_findings = json.loads(long_result.stdout)['long.qmd']
        assert len(long_findings) == 1 and long_findings[0]['Match'] == 'TODO'
        report = dict(useful=useful, false_positives=false_positives, missing=missing,
                      corpus_files=len(expected), corpus_words=words, corpus_lines=lines,
                      corpus_seconds=elapsed, per_rule=per_rule,
                      missed_cases=missed_cases,
                      long_words=len((project / 'long.qmd').read_text().split()),
                      long_bytes=(project / 'long.qmd').stat().st_size,
                      long_seconds=long_seconds, long_findings=len(long_findings))
        (root / 'evaluation.json').write_text(json.dumps(report, indent=2) + '\n')
        print(json.dumps(report, indent=2), flush=True)
        # Vale 3.24 accepts a definition later in the same paragraph: retain
        # this negative finding as a measured limitation, not a passing case.
        known_misses = [(name, *case) for name, targets in expected.items() for case in targets if case[-1] == 'ABC']
        assert not false_positives and Counter(missed_cases) == Counter(known_misses), 'Corpus findings differ from labelled cases/known misses'
        run('unit', [args.emacs, '-Q', '--batch', '-L', 'site-elisp', '-L', 'test',
                     '-l', 'awc-writing-test', '-f', 'ert-run-tests-batch-and-exit'])
        run('integration', [args.emacs, '-Q', '--batch', '-l', 'test/awc-writing-integration.el'])
        if args.gui:
            # Reuse the existing exact-tag cleanup, never the ordinary user's server.
            from importlib.util import module_from_spec, spec_from_file_location
            spec = spec_from_file_location('gui_helpers', REPO / 'test/run-quarto-gui-check.py')
            helpers = module_from_spec(spec)
            spec.loader.exec_module(helpers)
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
                                    '--name', 'Disposable writing QA', '--load',
                                    str(REPO / 'test/awc-writing-gui-check.el')], timeout=120)
                gui_report = json.loads((gui / 'result.json').read_text())
                assert gui_report['status'] == 'passed', gui_report
                for extension in ('md', 'qmd', 'org'):
                    validate_png(gui / f'{extension}.png')
                    validate_png(gui / f'mode-cancelled-{extension}.png')
                assert not helpers.owned_helpers(gui), 'Graphical helpers survived exit'
                assert not (gui / 'server/disposable-gui').exists(), 'GUI socket survived exit'
            finally:
                helpers.stop_owned_helpers(gui)
        success = True
        print(f'PASS writing QA; evidence: {root}', flush=True)
    finally:
        if success and not args.keep:
            shutil.rmtree(root)
        elif not success:
            print(f'FAILED; evidence retained: {root}', flush=True)


if __name__ == '__main__':
    main()
