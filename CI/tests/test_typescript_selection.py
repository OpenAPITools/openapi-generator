import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[2]
BASH = os.environ.get("CI_TEST_BASH") or shutil.which("bash")


def git_environment():
    environment = dict(os.environ)
    count = int(environment.pop("GIT_CONFIG_COUNT", "0"))
    for index in range(count):
        environment.pop(f"GIT_CONFIG_KEY_{index}", None)
        environment.pop(f"GIT_CONFIG_VALUE_{index}", None)
    return environment


@unittest.skipUnless(BASH and shutil.which("node") and shutil.which("git"),
                     "TypeScript selection requires the existing Bash, Node and Git runtimes")
class TypeScriptSelectionTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        subprocess.run(["git", "init", "--quiet", str(self.root)], check=True, env=git_environment())
        for sample in ("first sample", "second"):
            directory = self.root / "samples" / sample
            directory.mkdir(parents=True)
            for filename, content in (("tsconfig.json", "{}"), ("package.json", "{}"),
                                      (".openapi-generator-ignore", "")):
                (directory / filename).write_text(content, encoding="utf-8")
        subprocess.run(["git", "add", "samples"], cwd=self.root, check=True, env=git_environment())
        self.tools = self.root / "tools"
        self.tools.mkdir()
        npm = self.tools / "npm"
        npm.write_text('#!/bin/sh\nprintf "%s\\n" "$PWD" >> "$CI_NPM_CALLS"\n', encoding="utf-8")
        npm.chmod(0o755)
        self.calls = self.root / "calls"

    def run_script(self, *args):
        environment = git_environment()
        environment["PATH"] = str(self.tools) + os.pathsep + environment["PATH"]
        environment["CI_NPM_CALLS"] = self.calls.as_posix()
        return subprocess.run([BASH, str(ROOT / "bin" / "ts-typecheck-all.sh"), *args],
                              cwd=self.root, env=environment, text=True, capture_output=True)

    def test_empty_selection_does_not_install_or_typecheck(self):
        result = self.run_script("--samples-json", "[]")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertFalse(self.calls.exists())

    def test_selected_directory_preserves_spaces(self):
        result = self.run_script("--samples-json", json.dumps(["samples/first sample"]))
        self.assertEqual(result.returncode, 0, result.stderr)
        calls = self.calls.read_text(encoding="utf-8").splitlines()
        self.assertEqual(len(calls), 2)
        self.assertTrue(all(call.endswith("/samples/first sample") for call in calls), calls)

    def test_default_still_discovers_all_samples(self):
        result = self.run_script()
        self.assertEqual(result.returncode, 0, result.stderr)
        calls = self.calls.read_text(encoding="utf-8").splitlines()
        self.assertEqual(len(calls), 4)
        self.assertTrue(any(call.endswith("/samples/second") for call in calls))

    def test_default_preserves_existing_metadata_presence_rule(self):
        directory = self.root / "samples" / "new"
        directory.mkdir()
        (directory / "tsconfig.json").write_text("{}", encoding="utf-8")
        subprocess.run(["git", "add", "samples/new/tsconfig.json"], cwd=self.root, check=True,
                       env=git_environment())
        (directory / "package.json").write_text("{}", encoding="utf-8")
        (directory / ".openapi-generator-ignore").write_text("", encoding="utf-8")
        result = self.run_script()
        self.assertEqual(result.returncode, 0, result.stderr)
        calls = self.calls.read_text(encoding="utf-8").splitlines()
        self.assertEqual(len(calls), 6)
        self.assertTrue(any(call.endswith("/samples/new") for call in calls))

    def test_invalid_selection_fails_before_npm(self):
        for value in ('["samples/missing"]', '{"sample":"samples/second"}', 'not-json'):
            with self.subTest(value=value):
                result = self.run_script("--samples-json", value)
                self.assertNotEqual(result.returncode, 0)
                self.assertFalse(self.calls.exists())


if __name__ == "__main__":
    unittest.main()
