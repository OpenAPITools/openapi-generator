#!/usr/bin/env python3
"""Script to run mypy's test suite against this version of typeshed."""

import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

if __name__ == "__main__":
    with tempfile.TemporaryDirectory() as tempdir:
        dirpath = Path(tempdir)
        subprocess.run(["git", "clone", "--depth", "1", "git://github.com/python/mypy", dirpath / "mypy"], check=True)

        subprocess.run(["python2.7", "-m", "pip", "install", "--user", "typing"], check=True)
        subprocess.run(
            [sys.executable, "-m", "pip", "install", "-U", "-r", dirpath / "mypy/test-requirements.txt"], check=True
        )
        shutil.copytree("stdlib", dirpath / "mypy/mypy/typeshed/stdlib")
        shutil.copytree("third_party", dirpath / "mypy/mypy/typeshed/third_party")
        try:
            subprocess.run([sys.executable, "runtests.py", "typeshed-ci"], cwd=dirpath / "mypy", check=True)
        except subprocess.CalledProcessError as e:
            print("mypy tests failed", file=sys.stderr)
            sys.exit(e.returncode)
        else:
            print("mypy tests succeeded", file=sys.stderr)
            sys.exit(0)
