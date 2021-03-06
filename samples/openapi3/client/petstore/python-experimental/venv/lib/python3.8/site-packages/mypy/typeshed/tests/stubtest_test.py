#!/usr/bin/env python3
"""Test typeshed using stubtest

stubtest is a script in the mypy project that compares stubs to the actual objects at runtime.
Note that therefore the output of stubtest depends on which Python version it is run with.
In typeshed CI, we run stubtest with each currently supported Python minor version, except 2.7.

We pin the version of mypy / stubtest we use in .travis.yml so changes to those don't break
typeshed CI.

"""

import subprocess
import sys
from pathlib import Path


def run_stubtest(typeshed_dir: Path) -> int:
    whitelist_dir = typeshed_dir / "tests" / "stubtest_whitelists"
    version_whitelist = "py{}{}.txt".format(sys.version_info.major, sys.version_info.minor)
    platform_whitelist = "{}.txt".format(sys.platform)
    combined_whitelist = "{}-py{}{}.txt".format(sys.platform, sys.version_info.major, sys.version_info.minor)

    ignore_unused_whitelist = "--ignore-unused-whitelist" in sys.argv[1:]

    cmd = [
        sys.executable,
        "-m",
        "mypy.stubtest",
        # Use --ignore-missing-stub, because if someone makes a correct addition, they'll need to
        # also make a whitelist change and if someone makes an incorrect addition, they'll run into
        # false negatives.
        "--ignore-missing-stub",
        "--check-typeshed",
        "--custom-typeshed-dir",
        str(typeshed_dir),
        "--whitelist",
        str(whitelist_dir / "py3_common.txt"),
        "--whitelist",
        str(whitelist_dir / version_whitelist),
    ]
    if ignore_unused_whitelist:
        cmd += ["--ignore-unused-whitelist"]
    if (whitelist_dir / platform_whitelist).exists():
        cmd += [
            "--whitelist",
            str(whitelist_dir / platform_whitelist),
        ]
    if (whitelist_dir / combined_whitelist).exists():
        cmd += [
            "--whitelist",
            str(whitelist_dir / combined_whitelist),
        ]
    if sys.version_info < (3, 9):
        # As discussed in https://github.com/python/typeshed/issues/3693, we only aim for
        # positional-only arg accuracy for the latest Python version.
        cmd += ["--ignore-positional-only"]
    try:
        print(" ".join(cmd), file=sys.stderr)
        subprocess.run(cmd, check=True)
    except subprocess.CalledProcessError as e:
        print(
            "\nNB: stubtest output depends on the Python version (and system) it is run with. "
            "See README.md for more details.\n"
            "NB: We only check positional-only arg accuracy for Python 3.9.\n"
            "\nCommand run was: {}\n".format(" ".join(cmd)),
            file=sys.stderr,
        )
        print("stubtest failed", file=sys.stderr)
        return e.returncode
    else:
        print("stubtest succeeded", file=sys.stderr)
        return 0


if __name__ == "__main__":
    sys.exit(run_stubtest(typeshed_dir=Path(".")))
