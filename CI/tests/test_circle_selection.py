import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[2]
BASH = os.environ.get("CI_TEST_BASH") or shutil.which("bash")
RUBY = ["samples/client/petstore/" + name for name in
        ("ruby", "ruby-faraday", "ruby-httpx", "ruby-autoload")]


@unittest.skipUnless(BASH and shutil.which("git"), "Requires existing Bash and Git")
class CircleSelectionTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(dir=ROOT / "CI" / "tests")
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.source = self.root / "source"
        self.source.mkdir()
        self.git("init", "--quiet", cwd=self.source)
        self.git("config", "user.name", "CI tests", cwd=self.source)
        self.git("config", "user.email", "ci-tests@example.invalid", cwd=self.source)
        (self.source / "README.md").write_text("base\n", encoding="utf-8")
        self.git("add", ".", cwd=self.source)
        self.git("commit", "--quiet", "-m", "base", cwd=self.source)
        self.base = self.git("rev-parse", "HEAD", cwd=self.source)
        for sample in RUBY:
            directory = self.source / sample
            directory.mkdir(parents=True)
            (directory / "pom.xml").write_text("<project/>\n", encoding="utf-8")
        self.git("add", ".", cwd=self.source)
        self.git("commit", "--quiet", "-m", "samples", cwd=self.source)
        self.head = self.git("rev-parse", "HEAD", cwd=self.source)
        self.set_merge(self.base, self.head)
        self.work = self.root / "work"
        self.git("clone", "--quiet", "--depth=1", self.source.as_uri(), str(self.work),
                 cwd=self.root)
        (self.work / "CI").mkdir()
        for name in ("select_changes.py", "circle_select.sh", "circle_parallel.sh"):
            shutil.copyfile(ROOT / "CI" / name, self.work / "CI" / name)
        suites = {
            "circle.node0": {"workflow": ".circleci/config.yml", "paths": []},
            "circle.node1": {"workflow": ".circleci/config.yml",
                             "paths": ["samples/client/petstore/perl"]},
            "circle.node2": {"workflow": ".circleci/config.yml",
                             "paths": ["samples/client/petstore/cpp-restsdk/client"]},
            "circle.node3": {"workflow": ".circleci/config.yml",
                             "matrix": {"sample": RUBY}},
        }
        (self.work / "CI" / "change-scopes.json").write_text(
            json.dumps({"version": 1, "suites": suites}), encoding="utf-8")
        self.tools = self.root / "tools"
        self.tools.mkdir()
        for name in ("circleci-agent", "mvn"):
            script = self.tools / name
            script.write_text('#!/bin/bash\nprintf "%s %s\\n" "$PWD" "$*" >> "$CI_CALLS"\n',
                              encoding="utf-8")
            script.chmod(0o755)
        self.env_file = self.root / "bash-env"
        self.env_file.touch()
        self.calls = self.root / "calls"
        self.env = dict(os.environ, CIRCLE_NODE_INDEX="1", CIRCLE_SHA1=self.head,
                        CIRCLE_PULL_REQUEST="https://github.com/OpenAPITools/openapi-generator/pull/7",
                        CIRCLE_BRANCH="feature", CI_FORCE_FULL="false", CI_SELECTION_MODE="enforce",
                        BASH_ENV=self.env_file.as_posix(), CI_CALLS=self.calls.as_posix())
        self.env["PATH"] = str(self.tools) + os.pathsep + self.env["PATH"]
        for key in ("CI_IS_PR", "CI_BASE_SHA", "CI_HEAD_SHA", "CIRCLE_SELECTED_SAMPLES_JSON",
                    "GITHUB_OUTPUT", "GITHUB_STEP_SUMMARY"):
            self.env.pop(key, None)

    def git(self, *args, cwd):
        return subprocess.run(["git", *args], cwd=cwd, check=True, text=True,
                              capture_output=True).stdout.strip()

    def set_merge(self, base, head):
        tree = self.git("rev-parse", head + "^{tree}", cwd=self.source)
        merge = self.git("commit-tree", tree, "-p", base, "-p", head, "-m", "PR merge",
                         cwd=self.source)
        self.git("update-ref", "refs/pull/7/merge", merge, cwd=self.source)

    def run_script(self, name="circle_select.sh", check=True):
        result = subprocess.run([BASH, "CI/" + name], cwd=self.work, env=self.env,
                                text=True, capture_output=True)
        if check:
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        return result

    def selection(self):
        self.run_script()
        return json.loads((self.work / ".circleci-selection.json").read_text(encoding="utf-8"))

    def test_verified_release_or_fork_pr_can_halt_unrelated_node(self):
        result = self.selection()
        self.assertFalse(result["has_changes"])
        self.assertIn("step halt", self.calls.read_text(encoding="utf-8"))

    def test_relevant_ruby_node_runs_without_halt(self):
        self.env["CIRCLE_NODE_INDEX"] = "3"
        self.assertEqual(self.selection()["samples"], RUBY)
        self.assertFalse(self.calls.exists())

    def test_cumulative_ruby_selection_survives_later_docs_commit(self):
        base = self.head
        (self.source / RUBY[2] / "pom.xml").write_text("<project>changed</project>\n",
                                                       encoding="utf-8")
        self.git("add", ".", cwd=self.source)
        self.git("commit", "--quiet", "-m", "Ruby input", cwd=self.source)
        (self.source / "README.md").write_text("later documentation\n", encoding="utf-8")
        self.git("add", ".", cwd=self.source)
        self.git("commit", "--quiet", "-m", "documentation", cwd=self.source)
        self.head = self.git("rev-parse", "HEAD", cwd=self.source)
        self.set_merge(base, self.head)
        self.git("fetch", "--quiet", "--depth=1", "origin", self.head, cwd=self.work)
        self.git("checkout", "--quiet", "--detach", self.head, cwd=self.work)
        self.env.update(CIRCLE_NODE_INDEX="3", CIRCLE_SHA1=self.head)
        self.assertEqual(self.selection()["samples"], [RUBY[2]])
        self.run_script("circle_parallel.sh")
        calls = self.calls.read_text(encoding="utf-8").splitlines()
        self.assertEqual(len(calls), 1)
        self.assertTrue(calls[0].endswith("/ruby-httpx integration-test"), calls)

    def test_missing_merge_ref_runs_full(self):
        self.git("update-ref", "-d", "refs/pull/7/merge", cwd=self.source)
        self.assertTrue(self.selection()["run_all"])
        self.assertFalse(self.calls.exists())

    def test_moved_pr_merge_ref_runs_full(self):
        self.set_merge(self.head, self.base)
        self.assertTrue(self.selection()["run_all"])

    def test_wrong_event_revision_runs_full(self):
        self.env["CIRCLE_SHA1"] = self.base
        self.assertTrue(self.selection()["run_all"])

    def test_non_pr_ignores_untrusted_inherited_metadata(self):
        self.env.update(CIRCLE_PULL_REQUEST="", CI_IS_PR="true",
                        CI_BASE_SHA=self.base, CI_HEAD_SHA=self.head)
        self.assertTrue(self.selection()["run_all"])

    def test_explicit_full_run(self):
        self.env["CI_FORCE_FULL"] = "true"
        self.assertTrue(self.selection()["run_all"])
        self.assertFalse(self.calls.exists())

    def test_default_shadow_keeps_unrelated_node_running(self):
        self.env.pop("CI_SELECTION_MODE")
        result = self.selection()
        self.assertTrue(result["has_changes"])
        self.assertTrue(result["run_all"])
        self.assertFalse(self.calls.exists())

    def test_invalid_mode_fails_instead_of_skipping(self):
        self.env["CI_SELECTION_MODE"] = "unexpected"
        self.assertNotEqual(self.run_script(check=False).returncode, 0)
        self.assertFalse(self.calls.exists())

    def test_pull_branch_metadata(self):
        self.env.update(CIRCLE_PULL_REQUEST="", CIRCLE_BRANCH="pull/7")
        self.assertFalse(self.selection()["has_changes"])

    def test_selector_failure_never_halts_successfully(self):
        (self.work / "CI" / "change-scopes.json").write_text("{}", encoding="utf-8")
        self.assertNotEqual(self.run_script(check=False).returncode, 0)
        self.assertFalse(self.calls.exists())

    def test_ruby_payload_is_individually_filtered(self):
        self.env.update(CIRCLE_NODE_INDEX="3",
                        CIRCLE_SELECTED_SAMPLES_JSON=json.dumps([RUBY[2]]))
        self.run_script("circle_parallel.sh")
        calls = self.calls.read_text(encoding="utf-8").splitlines()
        self.assertEqual(len(calls), 1)
        self.assertTrue(calls[0].endswith("/ruby-httpx integration-test"), calls)

    def test_default_ruby_payload_preserves_all_four(self):
        self.env["CIRCLE_NODE_INDEX"] = "3"
        self.run_script("circle_parallel.sh")
        self.assertEqual(len(self.calls.read_text(encoding="utf-8").splitlines()), 4)

    def test_invalid_ruby_json_fails_before_payload(self):
        self.env["CIRCLE_NODE_INDEX"] = "3"
        for selection in ('[]', '{}', '["../escape"]', '["$(touch unwanted)"]', 'invalid'):
            with self.subTest(selection=selection):
                self.env["CIRCLE_SELECTED_SAMPLES_JSON"] = selection
                self.assertNotEqual(self.run_script("circle_parallel.sh", check=False).returncode, 0)
                self.assertFalse(self.calls.exists())


@unittest.skipUnless(shutil.which("node"), "Requires existing Node and vendored YAML parser")
class CircleConfigTests(unittest.TestCase):
    def test_gate_precedes_all_expensive_setup_and_preserves_jobs(self):
        script = (
            "const y=require('./.github/.test/js-yaml.js');"
            "console.log(JSON.stringify(y.safeLoad(require('fs').readFileSync("
            "'.circleci/config.yml','utf8'))));"
        )
        config = json.loads(subprocess.check_output(["node", "-e", script], cwd=ROOT, text=True))
        self.assertEqual(config["parameters"]["full_run"], {"type": "boolean", "default": False})
        self.assertEqual(config["parameters"]["selection_mode"],
                         {"type": "string", "default": "shadow"})
        steps = config["commands"]["command_build_and_test"]["steps"]
        self.assertIn("Checkout source", steps[0]["run"]["name"])
        self.assertIn("circle_select.sh", steps[1]["run"]["command"])
        self.assertEqual(steps[1]["run"]["environment"]["CI_SELECTION_MODE"],
                         "<<pipeline.parameters.selection_mode>>")
        self.assertIn("restore_cache", steps[2])
        checkout = steps[0]["run"]["command"]
        self.assertIn('git checkout --detach "$CIRCLE_SHA1"', checkout)
        self.assertIn('"$(git rev-parse FETCH_HEAD)" = "$CIRCLE_SHA1"', checkout)
        self.assertIn("seq 1 5", checkout)
        self.assertIn("https://github.com/OpenAPITools/openapi-generator.git", checkout)
        self.assertEqual(config["workflows"]["build"]["jobs"], ["node0", "node1", "node2", "node3"])
        self.assertTrue(any("store_test_results" in step for step in steps))
        self.assertTrue(any("store_artifacts" in step for step in steps))


if __name__ == "__main__":
    unittest.main()
