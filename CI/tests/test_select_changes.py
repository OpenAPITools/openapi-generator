import importlib.util
import io
import json
import os
from pathlib import Path
import shutil
import stat
import subprocess
import unittest
from unittest import mock
import uuid


ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location("select_changes", ROOT / "CI" / "select_changes.py")
selector = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(selector)


def policy(matrix=None, **kwargs):
    suite = {
        "workflow": ".github/workflows/example.yaml",
        "matrix": matrix if matrix is not None else {
            "sample": ["samples/a/", "samples/b"],
            "os": ["ubuntu-latest", "macos-latest"],
            "runtime": [11, 17],
        },
    }
    suite.update(kwargs)
    return {"version": 1, "suites": {"example.build": suite}}


class SelectionTests(unittest.TestCase):
    def select(self, paths, manifest=None, **kwargs):
        return selector.select_changes(manifest or policy(), "example.build", paths, **kwargs)

    def test_no_changes_has_stable_empty_types(self):
        for paths in ([], ["README.md"], ["samples/ab/file"], ["samples/a-other/file"]):
            with self.subTest(paths=paths):
                result = self.select(paths)
                self.assertEqual(result["matrix"], {"include": []})
                self.assertEqual(result["samples"], [])
                self.assertIs(result["has_changes"], False)
                self.assertIs(result["run_all"], False)
                self.assertIsInstance(result["reasons"], list)

    def test_sample_only_keeps_every_runtime_and_literal_output(self):
        result = self.select(["samples/a/.openapi-generator/FILES"])
        self.assertEqual(result["samples"], ["samples/a/"])
        self.assertEqual(result["matrix"], {
            "sample": ["samples/a/"], "os": ["ubuntu-latest", "macos-latest"], "runtime": [11, 17],
        })
        self.assertTrue(result["has_changes"])

    def test_all_global_inputs_and_workflow_force_full(self):
        paths = [
            "CI/select_changes.py", "CI/change-scopes.json", "CI/tests/new/test.py",
            ".github/actions/compute-matrix/action.yaml", ".github/.test/ci-scopes.js",
            ".gitattributes", ".gitmodules", ".github/workflows/example.yaml",
        ]
        for path in paths:
            with self.subTest(path=path):
                result = self.select([path])
                self.assertTrue(result["run_all"])
                self.assertEqual(result["matrix"], policy()["suites"]["example.build"]["matrix"])

    def test_owned_group_and_shared_globs(self):
        manifest = policy(paths=["fixtures/"], shared_paths=["bin/**/build?.sh", "pom.xml"])
        for path in ["fixtures/deep/input", "bin/build1.sh", "bin/deep/build2.sh", "pom.xml"]:
            with self.subTest(path=path):
                self.assertTrue(self.select([path], manifest)["run_all"])
        for path in ["fixtures-extra/input", "bin/build12.sh", "bin/deep/build.sh", "pom.xml.backup"]:
            with self.subTest(path=path):
                self.assertFalse(self.select([path], manifest)["has_changes"])
        self.assertFalse(selector.path_matches("bin/deep/file.sh", "bin/*.sh"))
        self.assertTrue(selector.path_matches("bin/file.sh", "bin/**/*.sh"))

    def test_unusual_samples_are_not_patterns(self):
        for sample in ["samples/a[1]", "samples/a+b (x)", "samples/a.b", "samples/a'quote", "samples/a?b"]:
            manifest = policy({"sample": [sample]})
            self.assertTrue(self.select([sample + "/line\nbreak.txt"], manifest)["has_changes"])
            self.assertFalse(self.select(["samples/axb/file"], manifest)["has_changes"])

    def test_swift_include_and_exclude_are_filtered(self):
        matrix = {
            "sample": ["samples/alamofire", "samples/urlsession"],
            "os": ["macos-latest"],
            "include": [{"sample": "samples/alamofire", "os": "ubuntu-latest"}, {"extra": True}],
            "exclude": [{"sample": "samples/alamofire", "os": "macos-latest"}],
        }
        result = self.select(["samples/urlsession/file"], policy(matrix))
        self.assertEqual(result["matrix"]["sample"], ["samples/urlsession"])
        self.assertEqual(result["matrix"]["include"], [{"extra": True}])
        self.assertEqual(result["matrix"]["exclude"], [])
        self.assertEqual(matrix["sample"], ["samples/alamofire", "samples/urlsession"])

    def test_include_only_sample_does_not_expand_other_axes(self):
        matrix = {
            "sample": ["samples/a"], "os": ["macos-latest"],
            "include": [{"sample": "samples/b", "os": "ubuntu-latest", "runtime": 17}],
        }
        result = self.select(["samples/b/test"], policy(matrix))
        self.assertEqual(result["matrix"], {"include": matrix["include"]})
        self.assertEqual(result["samples"], ["samples/b"])
        only = {"include": [{"sample": "samples/a", "os": "linux"}, {"sample": "samples/b", "os": "mac"}]}
        self.assertEqual(self.select(["samples/b/test"], policy(only))["matrix"], {
            "include": [{"sample": "samples/b", "os": "mac"}],
        })

    def test_filtered_combinations_match_original_compatibility(self):
        matrix = {
            "sample": ["samples/a", "samples/b"], "os": ["linux", "mac"], "runtime": [11, 17],
            "exclude": [{"sample": "samples/a", "os": "mac", "runtime": 11}],
            "include": [
                {"coverage": True},
                {"sample": "samples/a", "os": "windows", "runtime": 17},
                {"sample": "samples/c", "os": "linux", "runtime": 21},
            ],
        }
        original = selector.expand_matrix(matrix)
        for samples in [["samples/a"], ["samples/b"], ["samples/c"], ["samples/a", "samples/c"]]:
            with self.subTest(samples=samples):
                result = self.select([sample + "/test" for sample in samples], policy(matrix))
                expected = [row for row in original if row["sample"] in samples]
                self.assertFalse(result["run_all"])
                self.assertEqual(
                    selector.combination_counts(selector.expand_matrix(result["matrix"])),
                    selector.combination_counts(expected),
                )

    def test_completely_excluded_sample_has_no_jobs(self):
        matrix = {"sample": ["samples/a", "samples/b"], "exclude": [{"sample": "samples/a"}]}
        self.assertFalse(self.select(["samples/a/test"], policy(matrix))["has_changes"])

    def test_unbound_include_row_falls_back_instead_of_inventing_jobs(self):
        matrix = {
            "sample": ["samples/a", "samples/b"], "os": ["mac"],
            "include": [{"os": "windows"}],
        }
        result = self.select(["samples/a/test"], policy(matrix))
        self.assertTrue(result["run_all"])
        self.assertIn("combinations", result["reasons"][0])

    def test_full_mode_preserves_original_exactly(self):
        matrix = {"sample": ["samples/a/"], "include": [{"sample": "samples/b", "os": "linux"}], "exclude": []}
        result = self.select(None, policy(matrix), full_reason="forced")
        self.assertEqual(result["matrix"], matrix)
        self.assertEqual(result["samples"], ["samples/a/", "samples/b"])
        result["matrix"]["sample"].append("not in policy")
        self.assertEqual(matrix["sample"], ["samples/a/"])

    def test_runtime_only_and_matrixless_scopes(self):
        manifest = policy({"python": ["3.10", "3.11"]}, paths=["samples/legacy"])
        self.assertTrue(self.select(["samples/legacy/test"], manifest)["run_all"])
        self.assertFalse(self.select(["samples/other/test"], manifest)["has_changes"])
        del manifest["suites"]["example.build"]["matrix"]
        self.assertEqual(self.select(["samples/legacy/test"], manifest)["matrix"], {})

    def test_circle_scope_without_workflow_and_manifest_metadata(self):
        manifest = policy(paths=["CI/circle_parallel.sh"])
        del manifest["suites"]["example.build"]["workflow"]
        manifest["notes"] = ["baseline ownership"]
        manifest["exclusions"] = [{"path": "samples/unsupported", "reason": "no runnable workflow"}]
        self.assertTrue(self.select(["CI/circle_parallel.sh"], manifest)["run_all"])
        self.assertEqual(self.select(["samples/a/test"], manifest)["samples"], ["samples/a/"])
        self.assertFalse(self.select(["README.md"], manifest)["has_changes"])

    def test_shadow_executes_full_with_filtered_diagnostics(self):
        result = self.select(["samples/a/test"], mode="shadow")
        self.assertTrue(result["run_all"])
        self.assertEqual(result["samples"], ["samples/a/", "samples/b"])
        self.assertEqual(result["proposed_selection"]["samples"], ["samples/a/"])
        result = self.select([], mode="shadow")
        self.assertTrue(result["has_changes"])
        self.assertFalse(result["proposed_selection"]["has_changes"])

    def test_shadow_historical_paths_avoid_unrelated_workflow_runs(self):
        manifest = policy(shadow_paths=["samples/legacy/**"])
        result = self.select(["docs/guide.md"], manifest, mode="shadow")
        self.assertFalse(result["has_changes"])
        self.assertFalse(result["run_all"])
        self.assertEqual(result["matrix"], {"include": []})
        self.assertFalse(result["proposed_selection"]["has_changes"])
        for path in ("samples/legacy/old-trigger", "samples/a/new-owner"):
            with self.subTest(path=path):
                result = self.select([path], manifest, mode="shadow")
                self.assertTrue(result["has_changes"])
                self.assertTrue(result["run_all"])
                self.assertEqual(result["samples"], ["samples/a/", "samples/b"])
        result = self.select(["samples/legacy/old-trigger"], manifest, mode="shadow")
        self.assertFalse(result["proposed_selection"]["has_changes"])

    def test_shadow_shared_changes_and_uncertainty_always_run_full(self):
        manifest = policy(shadow_paths=["samples/legacy/**"], shared_paths=["root-build.xml"])
        for path in ("root-build.xml", ".github/workflows/example.yaml", "CI/select_changes.py"):
            with self.subTest(path=path):
                self.assertTrue(self.select([path], manifest, mode="shadow")["run_all"])
        for reason in ("diff uncertainty: missing history", "non-PR event", "explicit full-run override"):
            with self.subTest(reason=reason):
                result = self.select(None, manifest, full_reason=reason, mode="shadow")
                self.assertTrue(result["has_changes"])
                self.assertTrue(result["run_all"])
        self.assertTrue(self.select(["docs/guide.md"], policy(shadow_paths=["**"]), mode="shadow")["run_all"])
        self.assertFalse(self.select(["docs/guide.md"], manifest, mode="enforce")["has_changes"])

    def test_invalid_config_fails_instead_of_running_full(self):
        bad = [
            {"version": 2, "suites": {}},
            policy({"sample": ["../outside"]}),
            policy({"sample": "samples/a"}),
            policy({"sample": ["samples/a"], "os": [{"nested": "unsupported"}]}),
            policy({"sample": ["samples/a"], "exclude": [{"unknown": "value"}]}),
            policy(paths=["/absolute"]),
            policy(shared_paths=["C:\\absolute"]),
            policy(shared_paths=["bin/[a-z].sh"]),
            policy(shared_paths="bin/**"),
            policy(shadow_paths="samples/**"),
            policy(discovery="unknown"),
            policy(discovery="typescript"),
        ]
        for manifest in bad:
            with self.subTest(manifest=manifest), self.assertRaises(selector.ConfigError):
                self.select(None, manifest, full_reason="forced")
        with self.assertRaises(selector.ConfigError):
            selector.select_changes(policy(), "unknown", [])
        with self.assertRaises(selector.ConfigError):
            self.select(None)
        with self.assertRaises(selector.ConfigError):
            self.select([], mode="wrong")

    def test_outputs_escape_newlines_and_keep_json_types(self):
        stream = io.StringIO()
        result = self.select(["samples/a/new\nline"])
        result["reasons"] = ["untrusted\nhas_changes=false"]
        with mock.patch("builtins.open", mock.mock_open()) as opened, mock.patch("sys.stdout", stream):
            selector.emit_outputs(result, "outputs")
        lines = "".join(call.args[0] for call in opened().write.call_args_list).splitlines()
        values = {key: json.loads(value) for key, value in (line.split("=", 1) for line in lines)}
        self.assertEqual(values, result)
        self.assertEqual(json.loads(stream.getvalue()), result)

    def test_provider_non_pr_and_force_override(self):
        for provider, env in [("github", {"GITHUB_EVENT_NAME": "push"}), ("github", {"GITHUB_EVENT_NAME": "workflow_dispatch"}), ("circleci", {})]:
            self.assertIsNone(selector.resolve_changes(provider, env)[0])
        with mock.patch.object(selector, "load_manifest", return_value=policy()), \
                mock.patch.object(selector, "resolve_changes") as resolve, \
                mock.patch.object(selector, "validate_targets"), \
                mock.patch.object(selector, "emit_outputs") as emit, \
                mock.patch.dict(os.environ, {"CI_FORCE_FULL": "true"}, clear=True):
            self.assertEqual(selector.main(["--suite", "example.build"]), 0)
            resolve.assert_not_called()
            self.assertTrue(emit.call_args.args[0]["run_all"])

    def test_invalid_event_and_invalid_manifest_visible(self):
        paths, reason = selector.resolve_changes("github", {"GITHUB_EVENT_NAME": "pull_request"})
        self.assertIsNone(paths)
        self.assertIn("diff uncertainty", reason)
        with mock.patch.object(selector, "load_manifest", side_effect=selector.ConfigError("bad policy")), \
                mock.patch("sys.stderr", io.StringIO()) as stderr:
            self.assertEqual(selector.main(["--suite", "example.build"]), 2)
            self.assertIn("bad policy", stderr.getvalue())


class GitTests(unittest.TestCase):
    def setUp(self):
        # Keep all synthetic repositories inside the checkout, never system temp.
        self.directory = ROOT / "CI" / "tests" / (".selector-test-" + uuid.uuid4().hex)
        self.directory.mkdir()
        self.addCleanup(self.remove_repository)
        self.repo = self.directory / "repo"
        self.repo.mkdir()
        self.run_git("init", "--quiet")
        self.run_git("config", "user.email", "ci-tests@example.invalid")
        self.run_git("config", "user.name", "CI tests")
        self.run_git("config", "core.autocrlf", "false")
        self.write("README.md", "initial\n")
        self.base = self.commit("initial")
        self.git = selector.Git(self.repo)

    def remove_repository(self):
        def writable_retry(function, path, exc_info):
            os.chmod(path, stat.S_IWRITE)
            function(path)
        shutil.rmtree(self.directory, onerror=writable_retry)

    def run_git(self, *args, cwd=None):
        result = subprocess.run(
            ["git", *args], cwd=cwd or self.repo, check=True,
            stdout=subprocess.PIPE, stderr=subprocess.PIPE,
        )
        return result.stdout.decode("utf-8").strip()

    def write(self, name, content="content\n"):
        path = self.repo / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8")

    def commit(self, message):
        self.run_git("add", "--all")
        self.run_git("commit", "--quiet", "-m", message)
        return self.run_git("rev-parse", "HEAD")

    def test_cumulative_multiple_commits_and_no_net_changes(self):
        self.write("samples/a/first")
        first = self.commit("first PR commit")
        self.write("samples/b/second")
        head = self.commit("second PR commit")
        self.assertEqual(selector.pr_changes(self.git, self.base, head), ["samples/a/first", "samples/b/second"])
        self.run_git("revert", "--no-edit", head)
        head = self.run_git("rev-parse", "HEAD")
        self.assertEqual(selector.pr_changes(self.git, self.base, head), ["samples/a/first"])
        self.run_git("revert", "--no-edit", first)
        head = self.run_git("rev-parse", "HEAD")
        self.assertEqual(selector.pr_changes(self.git, self.base, head), [])

    def test_base_advancement_is_not_pr_change(self):
        self.run_git("checkout", "-q", "-b", "pr")
        self.write("samples/a/pr")
        head = self.commit("PR")
        self.run_git("checkout", "-q", "-b", "base-advanced", self.base)
        self.write("samples/b/base")
        advanced = self.commit("base advanced")
        self.run_git("checkout", "-q", "pr")
        self.assertEqual(selector.pr_changes(self.git, advanced, head), ["samples/a/pr"])

    def test_verified_merge_includes_merge_only_effects(self):
        self.write("samples/a/pr")
        head = self.commit("PR with unicode \u2603")
        self.run_git("checkout", "-q", "-b", "advanced", self.base)
        self.write("samples/b/base")
        advanced = self.commit("base")
        self.run_git("merge", "--no-ff", "--no-commit", head)
        self.write("samples/c/merge-only")
        merge = self.commit("synthetic merge")
        self.assertEqual(selector.pr_changes(self.git, advanced, head, merge, True), [
            "samples/a/pr", "samples/c/merge-only",
        ])
        with self.assertRaises(selector.DiffUncertain):
            selector.pr_changes(self.git, self.base, head, merge, True)

    def test_rename_deletion_and_mode_changes(self):
        self.write("samples/a/old.txt")
        self.write("samples/a/deleted.txt")
        self.write("samples/a/script.sh")
        base = self.commit("fixtures")
        self.run_git("mv", "samples/a/old.txt", "samples/a/new [odd] 'name.txt")
        self.run_git("rm", "samples/a/deleted.txt")
        self.run_git("update-index", "--chmod=+x", "samples/a/script.sh")
        self.run_git("commit", "-q", "-m", "rename delete chmod")
        head = self.run_git("rev-parse", "HEAD")
        self.assertEqual(selector.pr_changes(self.git, base, head), [
            "samples/a/deleted.txt", "samples/a/new [odd] 'name.txt", "samples/a/old.txt", "samples/a/script.sh",
        ])

    def test_nul_parser_handles_newlines_and_non_utf8(self):
        with mock.patch.object(self.git, "run", return_value=b"samples/a/line\nbreak\0samples/a/\xff\0"):
            self.assertEqual(self.git.diff(self.base, self.base), {
                "samples/a/line\nbreak", "samples/a/\udcff",
            })
        with mock.patch.object(self.git, "run", return_value=b"unterminated"), self.assertRaises(selector.DiffUncertain):
            self.git.diff(self.base, self.base)

    def test_more_than_api_limit_of_changed_files(self):
        for number in range(305):
            self.write(f"samples/a/{number}.txt")
        head = self.commit("large PR")
        self.assertEqual(len(selector.pr_changes(self.git, self.base, head)), 305)

    def test_missing_history_and_checkout_mismatch_are_full(self):
        env = {"CI_IS_PR": "true", "CI_BASE_SHA": "0" * 40, "CI_HEAD_SHA": self.base}
        changes, reason = selector.resolve_changes("circleci", env, self.repo)
        self.assertIsNone(changes)
        self.assertIn("diff uncertainty", reason)
        self.write("samples/a/pr")
        head = self.commit("PR")
        env.update(CI_BASE_SHA=self.base, CI_HEAD_SHA=head, CIRCLE_SHA1=self.base)
        changes, reason = selector.resolve_changes("circleci", env, self.repo)
        self.assertIsNone(changes)
        self.assertIn("checkout", reason)
        env["CIRCLE_SHA1"] = head
        self.assertEqual(selector.resolve_changes("circleci", env, self.repo), (["samples/a/pr"], None))
        env["CI_BASE_SHA"] = "--upload-pack=malicious"
        self.assertIn("invalid full commit SHA", selector.resolve_changes("circleci", env, self.repo)[1])

    def test_github_event_uses_verified_head_and_event_checkout(self):
        self.write("samples/a/pr")
        head = self.commit("PR")
        event = self.directory / "event.json"
        event.write_text(json.dumps({"pull_request": {"base": {"sha": self.base}, "head": {"sha": head}}}), encoding="utf-8")
        env = {"GITHUB_EVENT_NAME": "pull_request", "GITHUB_EVENT_PATH": str(event), "GITHUB_SHA": head}
        self.assertEqual(selector.resolve_changes("github", env, self.repo), (["samples/a/pr"], None))
        env["GITHUB_SHA"] = self.base
        self.assertIsNone(selector.resolve_changes("github", env, self.repo)[0])

    def test_shallow_checkout_fetches_exact_history(self):
        self.write("samples/a/first")
        self.commit("first PR commit")
        self.write("samples/b/last")
        head = self.commit("last PR commit")
        clone = self.directory / "shallow"
        self.run_git("clone", "--quiet", "--depth=1", self.repo.as_uri(), str(clone))
        self.assertEqual(self.run_git("rev-parse", "--is-shallow-repository", cwd=clone), "true")
        self.assertEqual(selector.pr_changes(selector.Git(clone), self.base, head), [
            "samples/a/first", "samples/b/last",
        ])

    def test_disconnected_history_does_not_guess(self):
        self.run_git("checkout", "--orphan", "disconnected")
        self.run_git("rm", "-rf", ".")
        self.write("samples/a/new-root")
        head = self.commit("unrelated history")
        with self.assertRaises(selector.DiffUncertain):
            selector.pr_changes(self.git, self.base, head)

    def test_only_executed_sample_directories_must_exist(self):
        self.write("samples/a/test")
        selected = selector.select_changes(policy(), "example.build", ["samples/a/test"])
        selector.validate_targets(selected, self.repo)
        skipped = selector.select_changes(policy(), "example.build", [])
        selector.validate_targets(skipped, self.repo)
        for mode in ("enforce", "shadow"):
            result = selector.select_changes(policy(), "example.build", ["samples/b/deleted"], mode=mode)
            with self.assertRaisesRegex(selector.ConfigError, "update ownership"):
                selector.validate_targets(result, self.repo)
        full = selector.select_changes(policy(), "example.build", full_reason="forced")
        with self.assertRaises(selector.ConfigError):
            selector.validate_targets(full, self.repo)

    def test_typescript_discovery_tracks_new_eligible_roots(self):
        for root in ("samples/typescript/a", "samples/other-ts/b [literal]", "samples/not-eligible"):
            self.write(root + "/tsconfig.json", "{}")
            self.write(root + "/package.json", "{}")
        for root in ("samples/typescript/a", "samples/other-ts/b [literal]"):
            self.write(root + "/.openapi-generator-ignore")
        self.commit("tracked TypeScript roots")
        self.write("samples/not-eligible/.openapi-generator-ignore")
        for name in ("tsconfig.json", "package.json", ".openapi-generator-ignore"):
            self.write("samples/untracked/" + name)
        self.assertEqual(selector.discover_typescript_samples(self.repo), [
            "samples/other-ts/b [literal]", "samples/typescript/a",
        ])
        self.run_git("add", "samples/not-eligible/.openapi-generator-ignore")
        self.assertIn("samples/not-eligible", selector.discover_typescript_samples(self.repo))

    def test_typescript_selection_retains_runtime_only_matrix(self):
        for root in ("samples/ts/a", "samples/ts/b"):
            for name in ("tsconfig.json", "package.json", ".openapi-generator-ignore"):
                self.write(root + "/" + name)
        self.commit("TypeScript samples")
        manifest = policy({"node-version": [20]}, discovery="typescript")
        result = selector.select_changes(manifest, "example.build", ["samples/ts/a/test"], cwd=self.repo)
        self.assertEqual(result["samples"], ["samples/ts/a"])
        self.assertEqual(result["matrix"], {"node-version": [20]})
        self.assertFalse(result["run_all"])
        skipped = selector.select_changes(manifest, "example.build", ["README.md"], cwd=self.repo)
        self.assertFalse(skipped["has_changes"])
        self.assertEqual(skipped["matrix"], {"include": []})
        for kwargs in ({"full_reason": "forced"}, {"changed_paths": [], "mode": "shadow"}):
            full = selector.select_changes(manifest, "example.build", cwd=self.repo, **kwargs)
            self.assertEqual(full["samples"], ["samples/ts/a", "samples/ts/b"])
            self.assertEqual(full["matrix"], {"node-version": [20]})
            self.assertTrue(full["run_all"])

    def test_typescript_discovery_failure_cannot_silently_skip(self):
        manifest = policy({"node-version": [20]}, discovery="typescript")
        with mock.patch.object(selector.Git, "run", side_effect=selector.DiffUncertain("git unavailable")):
            with self.assertRaisesRegex(selector.ConfigError, "cannot discover"):
                selector.select_changes(manifest, "example.build", [], cwd=self.repo)


if __name__ == "__main__":
    unittest.main()
