import json
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[2]


@unittest.skipUnless(shutil.which("node") and shutil.which("git"), "Scope coverage uses Node and Git")
class ScopeCoverageTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        for directory in (".github/.test", ".github/workflows", "bin/configs", "CI", "samples/client/example"):
            (self.root / directory).mkdir(parents=True)
        for filename in ("ci-scopes.js", "js-yaml.js"):
            shutil.copyfile(ROOT / ".github" / ".test" / filename,
                            self.root / ".github" / ".test" / filename)
        self.manifest = {
            "version": 1,
            "suites": {
                "samples-example.build": {
                    "workflow": ".github/workflows/samples-example.yaml",
                    "matrix": {"sample": ["samples/client/example"]},
                },
            },
        }
        (self.root / ".github/workflows/samples-example.yaml").write_text(
            """on:
  workflow_dispatch:
  push:
    branches: ['master', '[5-9]+.[0-9]+.x']
  pull_request:
jobs:
  setup:
    steps:
      - uses: ./.github/actions/compute-matrix
        with:
          suite: samples-example.build
  build:
    needs: setup
    if: ${{ needs.setup.outputs.build_changed == 'true' }}
    strategy:
      matrix: ${{ fromJSON(needs.setup.outputs.build_matrix) }}
""", encoding="utf-8")
        (self.root / "samples/client/example/package.json").write_text("{}", encoding="utf-8")
        subprocess.run(["git", "init", "--quiet", str(self.root)], check=True)

    def check(self):
        (self.root / "CI/change-scopes.json").write_text(json.dumps(self.manifest), encoding="utf-8")
        subprocess.run(["git", "add", "samples"], cwd=self.root, check=True)
        return subprocess.run(["node", ".github/.test/ci-scopes.js"], cwd=self.root,
                              text=True, capture_output=True)

    def test_registered_sample_passes(self):
        result = self.check()
        self.assertEqual(result.returncode, 0, result.stderr)

    def test_new_unregistered_build_root_fails(self):
        new = self.root / "samples/client/new"
        new.mkdir()
        (new / "pom.xml").write_text("<project/>", encoding="utf-8")
        result = self.check()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("Unowned sample samples/client/new", result.stderr)

    def test_exclusion_does_not_hide_nested_new_samples(self):
        self.manifest["exclusions"] = [{"path": "samples/client/new", "reason": "Unsupported baseline"}]
        new = self.root / "samples/client/new/nested"
        new.mkdir(parents=True)
        (new / "package.json").write_text("{}", encoding="utf-8")
        result = self.check()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("Unowned sample samples/client/new/nested", result.stderr)

    def test_local_package_dependency_must_be_declared(self):
        package = self.root / "samples/client/example/package.json"
        package.write_text(json.dumps({"dependencies": {"shared": "file:../shared"}}), encoding="utf-8")
        result = self.check()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("declare external local dependency samples/client/shared", result.stderr)
        self.manifest["suites"]["samples-example.build"]["shared_paths"] = ["samples/client/shared"]
        result = self.check()
        self.assertEqual(result.returncode, 0, result.stderr)


if __name__ == "__main__":
    unittest.main()
