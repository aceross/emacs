"""Meaningful provenance/failure tests, using only synthetic /tmp projects."""
import copy
import importlib.util
import json
import os
from pathlib import Path
import shutil
import signal
import subprocess
import sys
import tempfile
import time
import unittest
from unittest.mock import patch

RUNNER = Path(__file__).resolve().parents[1] / 'tools/paper/awc-paper.py'
spec = importlib.util.spec_from_file_location('awc_paper', RUNNER)
paper = importlib.util.module_from_spec(spec)
spec.loader.exec_module(paper)


class PaperRunnerTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix='awc-paper-unit-', dir='/tmp')
        self.root = Path(self.temp.name)
        (self.root / 'paper.qmd').write_text('A source manuscript.\n')
        (self.root / 'rules.txt').write_text('An intentional project rule.\n')
        (self.root / 'build.py').write_text("from pathlib import Path\nPath('paper.html').write_text('A rendered artifact')\n")
        self.cfg = {'version': 1, 'inputs': ['paper.qmd', 'rules.txt', '*.py'],
                    'checks': [{'id': 'build', 'argv': [sys.executable, 'build.py'],
                                'success': 'fresh-outputs', 'outputs': ['paper.html']}],
                    'tools': {'python': [sys.executable, '--version']}}
        self.path = self.root / paper.CONFIG
        self.write()

    def tearDown(self):
        self.temp.cleanup()

    def write(self):
        self.path.write_text(json.dumps(self.cfg))

    def cli(self, *args, code=0):
        result = subprocess.run([sys.executable, str(RUNNER), '--config', str(self.path), *args],
                                capture_output=True, text=True, timeout=10)
        self.assertEqual(result.returncode, code, result.stdout + result.stderr)
        return json.loads(result.stdout)

    def run_checks(self, code=0):
        self.write()
        return self.cli('check', '--execute', code=code)

    def git(self, *args):
        return subprocess.run(['git', '-c', 'core.hooksPath=/dev/null', '-c', 'commit.gpgsign=false',
                               '-c', 'user.name=Fixture QA', '-c', 'user.email=fixture@example.invalid', *args],
                              cwd=self.root, capture_output=True, text=True, check=True).stdout

    def test_status_and_freeze_never_run_git_hooks_or_filters(self):
        helper = self.root / 'git-helper'
        marker = self.root / 'git-helper-ran'
        for mechanism in ('fsmonitor', 'clean', 'process'):
            with self.subTest(mechanism=mechanism):
                if (self.root / '.git').exists():
                    shutil.rmtree(self.root / '.git')
                self.git('init', '-q')
                self.git('add', 'paper.qmd', paper.CONFIG, 'build.py', 'rules.txt')
                self.git('commit', '-qm', 'Synthetic baseline')
                helper.write_text('#!/bin/sh\nprintf "executed\\n" >> git-helper-ran\n'
                                  + {'fsmonitor': 'printf "fixture-token\\0"\n',
                                     'clean': 'cat\n', 'process': 'exit 1\n'}[mechanism])
                helper.chmod(0o700)
                key = 'core.fsmonitor' if mechanism == 'fsmonitor' else f'filter.fixture.{mechanism}'
                self.git('config', key, str(helper))
                if mechanism != 'fsmonitor':
                    (self.root / '.gitattributes').write_text('paper.qmd filter=fixture\n')
                source = self.root / 'paper.qmd'
                source.write_text(('X' if mechanism != 'clean' else 'Y') * (source.stat().st_size - 1) + '\n')
                index = (self.root / '.git/index').read_bytes()
                try:
                    status = self.cli('status')
                    frozen = self.cli('--expect', status['review_token'], 'freeze', '--label', 'Passive Git review')
                    self.assertFalse(marker.exists(), f'{mechanism} executed without --execute')
                    self.assertEqual((self.root / '.git/index').read_bytes(), index)
                    self.assertEqual(frozen['snapshot']['git']['head'], self.git('rev-parse', 'HEAD').strip())
                    self.assertTrue(frozen['snapshot']['git']['input_status'])
                    self.assertFalse((self.root / 'paper.html').exists())
                    # A positive control proves each configured helper would
                    # actually run through native worktree status.
                    subprocess.run(['git', 'status', '--porcelain'], cwd=self.root,
                                   capture_output=True, timeout=5)
                    self.assertTrue(marker.exists(), f'{mechanism} fixture did not exercise Git execution')
                finally:
                    self.git('config', '--unset', key)
                    marker.unlink(missing_ok=True)

    def test_git_provenance_retains_staged_unstaged_untracked_and_conflicted_inputs(self):
        # Raw object comparison must support both Git object formats.
        for algorithm in ('sha1', 'sha256'):
            with self.subTest(algorithm=algorithm):
                gitdir = self.root / '.git'
                if gitdir.exists():
                    shutil.rmtree(gitdir)
                self.git('init', '-q', f'--object-format={algorithm}')
                (self.root / 'paper.qmd').write_text('Committed manuscript.\n')
                self.git('add', '.')
                self.git('commit', '-qm', 'Synthetic baseline')
                self.assertFalse(self.cli('status')['snapshot']['git']['input_status'])
                (self.root / 'paper.qmd').write_text('Staged manuscript.\n')
                self.git('add', 'paper.qmd')
                staged = self.cli('status')['snapshot']['git']['input_status']
                self.assertIn('M  paper.qmd', staged)
                (self.root / 'paper.qmd').write_text('Further unsaved-to-index manuscript.\n')
                (self.root / 'new.py').write_text('# Declared untracked helper\n')
                dirty = self.cli('status')['snapshot']['git']['input_status']
                self.assertIn('MM paper.qmd', dirty)
                self.assertIn('?? new.py', dirty)
                self.git('update-index', '--chmod=+x', 'build.py')
                self.assertIn('MM build.py', self.cli('status')['snapshot']['git']['input_status'])
                blob = self.git('rev-parse', ':paper.qmd').strip()
                self.git('update-index', '--force-remove', 'paper.qmd')
                subprocess.run(['git', 'update-index', '--index-info'], cwd=self.root, text=True,
                               input=f'100644 {blob} 1\tpaper.qmd\n100644 {blob} 2\tpaper.qmd\n', check=True)
                self.assertIn('UU paper.qmd', self.cli('status')['snapshot']['git']['input_status'])
                (self.root / 'new.py').unlink()

    def test_nested_git_inputs_use_literal_names_and_ignore_cached_clean_flags(self):
        nested = self.root / 'nested project'
        nested.mkdir()
        source = nested / 'literal[1].qmd'
        source.write_text('Synthetic literal source.\n')
        config = nested / paper.CONFIG
        config.write_text(json.dumps({'version': 1, 'inputs': [source.name.replace('[', '[[]')]}))
        self.git('init', '-q')
        self.git('add', '.')
        self.git('commit', '-qm', 'Nested project baseline')
        root, cfg = paper.load_config(config)
        self.assertFalse(paper.status(root, config, cfg)['snapshot']['git']['input_status'])
        self.git('--literal-pathspecs', 'update-index', '--assume-unchanged', str(source.relative_to(self.root)))
        source.write_text('Changed literal source.\n')
        (nested / 'literal1.qmd').write_text('Unrelated pathspec candidate.\n')
        self.assertEqual(paper.status(root, config, cfg)['snapshot']['git']['input_status'], [' M literal[1].qmd'])

    def test_git_provenance_never_fetches_missing_promised_objects(self):
        self.git('init', '-q')
        self.git('add', '.')
        self.git('commit', '-qm', 'Synthetic partial-clone baseline')
        tree = self.git('rev-parse', 'HEAD^{tree}').strip()
        (self.root / '.git/objects' / tree[:2] / tree[2:]).rename(self.root / 'missing-tree')
        helper = self.root / 'remote-helper'
        marker = self.root / 'remote-helper-ran'
        helper.write_text('#!/bin/sh\nprintf "executed\\n" >> remote-helper-ran\nexit 1\n')
        helper.chmod(0o700)
        for key, value in (('core.repositoryformatversion', '1'), ('extensions.partialclone', 'fixture'),
                           ('remote.fixture.url', 'ext::' + str(helper)), ('remote.fixture.promisor', 'true'),
                           ('protocol.ext.allow', 'always')):
            self.git('config', key, value)
        result = self.cli('status')['snapshot']['git']
        self.assertFalse(marker.exists(), 'Passive provenance launched a remote helper')
        self.assertEqual(result['status'], 'unavailable')
        # The explicit control is local: its transport only writes a marker.
        subprocess.run(['git', 'ls-tree', 'HEAD'], cwd=self.root, capture_output=True, timeout=5)
        self.assertTrue(marker.exists())

    def test_output_aliases_are_refused_before_source_or_config_can_be_overwritten(self):
        for alias in ('./paper.qmd', './/paper.qmd', './.awc-paper.json'):
            with self.subTest(alias=alias):
                self.cfg['checks'][0].update(outputs=[alias], argv=[sys.executable, '-c',
                    "from pathlib import Path;Path('paper.qmd').write_text('Overwritten')"])
                self.write()
                original = (self.root / 'paper.qmd').read_bytes()
                self.assertIn('overlap', self.cli('status', code=2)['error'])
                self.assertIn('overlap', self.cli('check', '--execute', code=2)['error'])
                self.assertEqual((self.root / 'paper.qmd').read_bytes(), original)
                self.assertFalse((self.root / paper.STATE / 'latest.json').exists())

    def test_nonoverlapping_output_and_word_source_aliases_are_canonical(self):
        self.cfg['checks'][0]['outputs'] = ['./paper.html']
        self.cfg['checks'].append({'id': 'words', 'type': 'word-count', 'files': ['./paper.qmd'],
                                  'argv': [sys.executable, '-c', "print('{\"blocks\":[]}')"]})
        result = self.run_checks()
        self.assertTrue(result['ready'])
        self.assertEqual(result['plan']['checks'][0]['outputs'], ['paper.html'])
        self.assertEqual(result['plan']['checks'][1]['files'], ['paper.qmd'])
        self.assertEqual(result['checks'][1]['commands'][0][-1], 'paper.qmd')

    def test_status_never_executes_project_code_or_creates_state(self):
        self.cli('status')
        self.assertFalse((self.root / paper.STATE).exists())
        self.assertFalse((self.root / 'paper.html').exists())
        self.assertIn('--execute', self.cli('check', code=2)['error'])
        self.assertFalse((self.root / 'paper.html').exists())

    def test_malformed_config_shapes_fail_before_execution(self):
        for key, value in (('checks', ['not an object']), ('manual', None), ('tools', [])):
            original = copy.deepcopy(self.cfg)
            self.cfg[key] = value
            self.write()
            self.assertIn('error', self.cli('check', '--execute', code=2))
            self.assertFalse((self.root / 'paper.html').exists())
            self.cfg = original

    def test_real_artifact_and_tool_provenance(self):
        result = self.run_checks()
        self.assertTrue(result['ready'])
        self.assertEqual(result['outputs']['paper.html']['sha256'], paper.sha(self.root / 'paper.html'))
        self.assertTrue(result['run']['tools']['python']['version'])
        self.assertTrue(result['snapshot']['executables']['build']['sha256'])
        self.assertTrue(result['snapshot']['runner_sha256'])

    def test_source_rules_script_and_config_changes_invalidate(self):
        for name in ('paper.qmd', 'rules.txt', 'build.py', paper.CONFIG):
            with self.subTest(name=name):
                self.run_checks()
                original = (self.root / name).read_text()
                (self.root / name).write_text(original + (' ' if name == paper.CONFIG else '\n# Changed\n'))
                self.assertEqual(self.cli('status')['checks'][0]['state'], 'stale')
                (self.root / name).write_text(original)

    def test_glob_addition_and_missing_input_are_detected(self):
        self.run_checks()
        (self.root / 'added.py').write_text('# new validation helper')
        self.assertEqual(self.cli('status')['checks'][0]['state'], 'stale')
        (self.root / 'rules.txt').unlink()
        self.assertIn('rules.txt', self.cli('status')['snapshot']['missing'])
        self.cli('check', '--execute', code=2)

    def test_output_and_log_tampering_are_stale(self):
        result = self.run_checks()
        (self.root / 'paper.html').write_text('tampered')
        self.assertEqual(self.cli('status')['checks'][0]['state'], 'stale')
        result = self.run_checks()
        token = result['review_token']
        (self.root / result['checks'][0]['log']).write_text('tampered log')
        status = self.cli('status')
        self.assertEqual(status['checks'][0]['state'], 'stale')
        self.assertNotEqual(status['review_token'], token)

    def test_zero_exit_with_absent_or_old_output_fails(self):
        self.cfg['checks'][0]['argv'] = [sys.executable, '-c', 'pass']
        for old in (False, True):
            if old:
                (self.root / 'paper.html').write_text('old artifact')
            result = self.run_checks(code=1)
            self.assertEqual(result['checks'][0]['state'], 'failed')

    def test_zero_exit_with_configured_failure_diagnostic_fails(self):
        self.cfg['checks'][0].update(argv=[sys.executable, '-c', "print('paper.qmd:1:1: citation missing not found')"],
                                     success='exit-code', outputs=[], reject=['citation.*not found'])
        self.assertEqual(self.run_checks(code=1)['checks'][0]['state'], 'failed')

    def test_missing_executable_and_failed_command_stay_distinct(self):
        check = self.cfg['checks'][0]
        check.update(success='exit-code', outputs=[], argv=['awc-nonexistent-manuscript-tool'])
        self.assertEqual(self.run_checks(code=1)['checks'][0]['state'], 'unavailable')
        check['argv'] = [sys.executable, '-c', 'raise SystemExit(7)']
        result = self.run_checks(code=1)['checks'][0]
        self.assertEqual((result['state'], result['exit']), ('failed', 7))

    def test_failed_required_version_probe_prevents_readiness(self):
        self.cfg['tools'] = {'broken': [sys.executable, '-c', 'raise SystemExit(1)']}
        self.assertFalse(self.run_checks(code=1)['ready'])

    def test_empty_checks_skips_and_not_applicable_are_not_silent_passes(self):
        self.cfg['checks'] = []
        self.assertFalse(self.run_checks(code=1)['ready'])
        self.cfg['checks'] = [{'id': 'skip', 'argv': ['unused'], 'success': 'exit-code', 'skip': 'Explicitly deferred'}]
        self.cfg['manual'] = [{'id': 'na', 'description': 'Not needed', 'not_applicable': 'Fixture has no appendix'}]
        result = self.run_checks(code=1)
        self.assertEqual(result['checks'][0]['state'], 'skipped')
        self.assertEqual(result['manual'][0]['state'], 'not-applicable')

    def test_manual_review_binds_reviewer_note_inputs_and_outputs(self):
        self.cfg['manual'] = [{'id': 'review', 'description': 'Review the submitted artifact'}]
        self.run_checks(code=1)
        result = self.cli('verify', 'review', '--reviewer', 'Synthetic reviewer', '--note', 'Compared the fixture output with the source')
        self.assertTrue(result['ready'])
        self.assertEqual(result['manual'][0]['state'], 'manually-verified')
        (self.root / 'paper.html').write_text('changed')
        self.assertEqual(self.cli('status')['manual'][0]['state'], 'stale')

    def test_checkpoint_preserves_failures_and_never_replaces_old_manifest(self):
        self.cfg['checks'][0]['argv'] = [sys.executable, '-c', 'raise SystemExit(3)']
        self.run_checks(code=1)
        first = self.cli('freeze', '--label', 'Failure reviewed')
        path = self.root / first['checkpoint']
        saved = path.read_bytes()
        self.assertFalse(json.loads(saved)['ready'])
        self.assertEqual(json.loads(saved)['checks'][0]['state'], 'failed')
        second = self.cli('freeze', '--label', 'Second review')
        self.assertNotEqual(first['checkpoint'], second['checkpoint'])
        self.assertEqual(path.read_bytes(), saved)

    def test_changed_review_token_refuses_execution_and_freeze(self):
        result = self.cli('status')
        (self.root / 'paper.qmd').write_text('changed')
        self.cli('--expect', result['review_token'], 'check', '--execute', code=2)
        self.cli('--expect', result['review_token'], 'freeze', '--label', 'Old review', code=2)
        self.assertFalse((self.root / 'paper.html').exists())
        self.assertFalse((self.root / paper.STATE / 'checkpoints').exists())

    def test_atomic_replace_failure_preserves_prior_record(self):
        paper.atomic(self.root, paper.STATE + '/test.json', {'old': True})
        with patch.object(paper.os, 'replace', side_effect=OSError('interrupted replacement')):
            with self.assertRaises(OSError):
                paper.atomic(self.root, paper.STATE + '/test.json', {'new': True})
        self.assertEqual(json.loads((self.root / paper.STATE / 'test.json').read_text()), {'old': True})
        self.assertFalse(list((self.root / paper.STATE).glob('.pending-*')))

    def test_path_traversal_symlinks_and_input_output_overlap_are_refused(self):
        for pattern in ('../outside', '/etc/passwd', '.awc-paper/*', 'paper.html'):
            with self.subTest(pattern=pattern):
                self.cfg['inputs'] = [pattern]
                if pattern == '.awc-paper/*':
                    (self.root / paper.STATE).mkdir(exist_ok=True)
                    (self.root / paper.STATE / 'private').write_text('state')
                if pattern == 'paper.html':
                    (self.root / 'paper.html').write_text('old')
                self.write()
                self.cli('status', code=2)
        self.cfg['inputs'] = ['linked']
        (self.root / 'linked').symlink_to('/etc/hosts')
        self.write()
        self.cli('status', code=2)
        self.cfg['inputs'] = ['paper.qmd']
        self.write()
        shutil.rmtree(self.root / paper.STATE)
        (self.root / paper.STATE).symlink_to('/tmp')
        self.cli('freeze', '--label', 'refused', code=2)

    def test_word_count_ast_excludes_syntax_and_counts_defined_prose(self):
        ast = {'meta': {'x': {'t': 'Str', 'c': 'metadata'}}, 'blocks': [
            {'t': 'Header', 'c': [1, [], [{'t': 'Str', 'c': 'Body heading'}]]},
            {'t': 'Para', 'c': [{'t': 'Str', 'c': 'A clear result'}, {'t': 'Math', 'c': [[], 'math words']},
                                {'t': 'Cite', 'c': [[], [{'t': 'Str', 'c': 'citation'}]]},
                                {'t': 'Code', 'c': [[], 'code words']},
                                {'t': 'Link', 'c': [[], [{'t': 'Str', 'c': 'source label'}], ['target words', 'title']]}]},
            {'t': 'CodeBlock', 'c': [[], 'code words']}]}
        self.assertEqual(paper.word_count(ast), 7)

    def test_reviewed_plan_matches_executed_argv_without_implicit_execution(self):
        source = 'supplement with spaces é.qmd'
        (self.root / source).write_text('More synthetic prose.\n')
        (self.root / 'pandoc.py').write_text(
            "import json,sys\nfrom pathlib import Path\n"
            "with Path('invocations.jsonl').open('a') as out: out.write(json.dumps(sys.argv)+'\\n')\n"
            "print(json.dumps({'blocks':[{'t':'Para','c':[{'t':'Str','c':'two words'}]}]}))\n")
        self.cfg['inputs'].append(source)
        launcher = [sys.executable, 'pandoc.py', '--fixture', 'one argument']
        self.cfg['checks'] += [
            {'id': 'words', 'type': 'word-count', 'argv': launcher, 'files': ['paper.qmd', source]},
            {'id': 'deferred', 'argv': ['never-run'], 'success': 'exit-code', 'skip': 'Explicitly deferred'}]
        self.write()
        initial = self.cli('status')
        planned = initial['plan']['checks']
        expected = [launcher + ['--from=markdown', '--to=json', name] for name in ('paper.qmd', source)]
        self.assertEqual(planned[0]['commands'], [self.cfg['checks'][0]['argv']])
        self.assertEqual(planned[1]['commands'], expected)
        self.assertEqual(planned[2]['commands'], [])
        self.assertFalse((self.root / 'invocations.jsonl').exists())
        self.assertFalse((self.root / paper.STATE).exists())
        # A different source order changes the reviewed configuration/token.
        self.cfg['checks'][1]['files'].reverse()
        self.write()
        self.cli('--expect', initial['review_token'], 'check', '--execute', code=2)
        self.assertFalse((self.root / 'invocations.jsonl').exists())
        self.cfg['checks'][1]['files'].reverse()
        self.write()
        result = self.cli('--expect', initial['review_token'], 'check', '--execute', code=1)
        actual = [json.loads(line) for line in (self.root / 'invocations.jsonl').read_text().splitlines()]
        self.assertEqual(actual, [command[1:] for command in expected])
        self.assertEqual(result['checks'][1]['commands'], expected)
        self.assertEqual(result['checks'][1]['total'], 4)
        self.assertEqual(result['checks'][2]['state'], 'skipped')
        # The plan covers both sources; a failed first invocation records only
        # the command actually attempted, preserving partial-run provenance.
        (self.root / 'pandoc.py').write_text('raise SystemExit(9)\n')
        failed = self.cli('check', '--execute', code=1)
        self.assertEqual(failed['checks'][1]['state'], 'failed')
        self.assertEqual(failed['plan']['checks'][1]['commands'], expected)
        self.assertEqual(failed['checks'][1]['commands'], expected[:1])

    def test_word_limit_requires_dated_policy_and_reports_failure(self):
        self.cfg['checks'] = [{'id': 'words', 'type': 'word-count', 'files': ['paper.qmd'],
                               'argv': [sys.executable, '-c', "import json;print(json.dumps({'blocks':[{'t':'Para','c':[{'t':'Str','c':'one two three'}]}]}))"],
                               'limit': 2, 'policy': {'source': 'Fixture policy', 'date': '2026-10-19'}}]
        result = self.run_checks(code=1)
        self.assertEqual(result['checks'][0]['total'], 3)
        self.cfg['checks'][0]['policy'] = {}
        self.write()
        self.cli('status', code=2)

    def test_git_dirty_untracked_inputs_and_changed_head_are_recorded(self):
        def git(*args):
            return subprocess.run(['git', '-c', 'commit.gpgsign=false', *args], cwd=self.root, capture_output=True, text=True, check=True).stdout
        git('init', '-q')
        git('add', 'paper.qmd')
        git('-c', 'user.name=Fixture QA', '-c', 'user.email=fixture@example.invalid', 'commit', '-qm', 'Synthetic baseline')
        first = self.run_checks()
        self.assertTrue(first['snapshot']['git']['head'])
        self.assertTrue(first['snapshot']['git']['input_status'])
        git('add', 'rules.txt')
        git('-c', 'user.name=Fixture QA', '-c', 'user.email=fixture@example.invalid', 'commit', '-qm', 'Synthetic revision')
        self.assertEqual(self.cli('status')['checks'][0]['state'], 'stale')

    def slow(self, seconds=10, helper=False):
        script = 'import os,time\nfrom pathlib import Path\n'
        if helper:
            script += "import subprocess,sys\np=subprocess.Popen([sys.executable,'-c','import time;time.sleep(30)'])\nPath('helper.pid').write_text(str(p.pid))\n"
        script += f"Path('running.pid').write_text(str(os.getpid()))\ntime.sleep({seconds})\n"
        (self.root / 'build.py').write_text(script)
        self.cfg['checks'][0].update(success='exit-code', outputs=[])
        self.write()

    def start(self):
        proc = subprocess.Popen([sys.executable, str(RUNNER), '--config', str(self.path), 'check', '--execute'], stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
        deadline = time.monotonic() + 5
        while not (self.root / 'running.pid').exists() and time.monotonic() < deadline:
            time.sleep(0.02)
        self.assertTrue((self.root / 'running.pid').exists())
        self.addCleanup(lambda: proc.kill() if proc.poll() is None else None)
        return proc

    def assert_gone(self, pid):
        deadline = time.monotonic() + 3
        while time.monotonic() < deadline:
            stat = Path(f'/proc/{pid}/stat')
            if not stat.exists() or stat.read_text().rsplit(')', 1)[1].split()[0] == 'Z':
                return
            time.sleep(0.02)
        self.fail(f'Owned helper survived: {pid}')

    def test_cancellation_reaps_command_and_helper(self):
        self.slow(helper=True)
        proc = self.start()
        child = int((self.root / 'running.pid').read_text())
        helper = int((self.root / 'helper.pid').read_text())
        proc.send_signal(signal.SIGTERM)
        out, err = proc.communicate(timeout=5)
        self.assertEqual(proc.returncode, 1, err)
        self.assertEqual(json.loads(out)['checks'][0]['state'], 'cancelled')
        self.assert_gone(child)
        self.assert_gone(helper)

    def test_timeout_reaps_command(self):
        self.slow()
        self.cfg['checks'][0]['timeout'] = 0.2
        result = self.run_checks(code=1)
        self.assertIn('timed out', result['checks'][0]['detail'])
        self.assert_gone(int((self.root / 'running.pid').read_text()))

    def test_same_project_lock_and_source_change_during_check(self):
        self.slow(seconds=0.4)
        proc = self.start()
        self.assertEqual(self.cli('status')['checks'][0]['state'], 'running')
        self.assertIn('Another manuscript action', self.cli('check', '--execute', code=2)['error'])
        (self.root / 'paper.qmd').write_text('Changed while command was running')
        out, err = proc.communicate(timeout=5)
        self.assertEqual(proc.returncode, 1, err)
        self.assertEqual(json.loads(out)['checks'][0]['state'], 'stale')

    def test_interrupted_run_is_cancelled_even_while_freeze_holds_a_lock(self):
        record = {'snapshot': paper.snapshot(self.root, self.path, self.cfg),
                  'owner_pid': 99999999, 'checks': [{'id': 'build', 'state': 'running'}]}
        paper.atomic(self.root, paper.STATE + '/latest.json', record)
        self.assertEqual(self.cli('status')['checks'][0]['state'], 'cancelled')
        frozen = self.cli('freeze', '--label', 'Interrupted run reviewed')
        saved = json.loads((self.root / frozen['checkpoint']).read_text())
        self.assertEqual(saved['checks'][0]['state'], 'cancelled')

    def test_two_projects_run_independently(self):
        self.slow(seconds=0.8)
        proc = self.start()
        sibling = self.root / 'sibling project é'
        sibling.mkdir()
        for name in ('paper.qmd', 'rules.txt', 'build.py', paper.CONFIG):
            shutil.copyfile(self.root / name, sibling / name)
        (sibling / 'build.py').write_text("print('Independent sibling check')\n")
        result = subprocess.run([sys.executable, str(RUNNER), '--config', str(sibling / paper.CONFIG),
                                 'check', '--execute'], capture_output=True, text=True, timeout=5)
        self.assertEqual(result.returncode, 0, result.stdout)
        self.assertTrue(json.loads(result.stdout)['ready'])
        out, err = proc.communicate(timeout=5)
        self.assertEqual(proc.returncode, 0, err)
        self.assertTrue(json.loads(out)['ready'])


if __name__ == '__main__':
    unittest.main(verbosity=2)
