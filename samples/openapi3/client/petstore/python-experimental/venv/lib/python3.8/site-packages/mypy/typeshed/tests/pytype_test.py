#!/usr/bin/env python3
"""Test runner for typeshed.

Depends on pytype being installed.

If pytype is installed:
    1. For every pyi, do nothing if it is in pytype_exclude_list.txt.
    2. Otherwise, call 'pytype.io.parse_pyi'.
Option two will load the file and all the builtins, typeshed dependencies. This
will also discover incorrect usage of imported modules.
"""

import argparse
import os
import re
import sys
import traceback
from typing import List, Match, Optional, Sequence, Tuple

from pytype import config as pytype_config, io as pytype_io

TYPESHED_SUBDIRS = ["stdlib", "third_party"]


TYPESHED_HOME = "TYPESHED_HOME"
UNSET = object()  # marker for tracking the TYPESHED_HOME environment variable


def main() -> None:
    args = create_parser().parse_args()
    typeshed_location = args.typeshed_location or os.getcwd()
    subdir_paths = [os.path.join(typeshed_location, d) for d in TYPESHED_SUBDIRS]
    check_subdirs_discoverable(subdir_paths)
    files_to_test = determine_files_to_test(typeshed_location=typeshed_location, paths=args.files or subdir_paths)
    run_all_tests(
        files_to_test=files_to_test,
        typeshed_location=typeshed_location,
        print_stderr=args.print_stderr,
        dry_run=args.dry_run,
    )


def create_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Pytype/typeshed tests.")
    parser.add_argument("-n", "--dry-run", action="store_true", default=False, help="Don't actually run tests")
    # Default to '' so that symlinking typeshed subdirs in cwd will work.
    parser.add_argument("--typeshed-location", type=str, default="", help="Path to typeshed installation.")
    # Set to true to print a stack trace every time an exception is thrown.
    parser.add_argument(
        "--print-stderr", action="store_true", default=False, help="Print stderr every time an error is encountered."
    )
    parser.add_argument(
        "files",
        metavar="FILE",
        type=str,
        nargs="*",
        help="Files or directories to check. (Default: Check all files.)",
    )
    return parser


class PathMatcher:
    def __init__(self, patterns: Sequence[str]) -> None:
        patterns = [re.escape(os.path.join(*x.split("/"))) for x in patterns]
        self.matcher = re.compile(r"({})$".format("|".join(patterns))) if patterns else None

    def search(self, path: str) -> Optional[Match[str]]:
        if not self.matcher:
            return None
        return self.matcher.search(path)


def load_exclude_list(typeshed_location: str) -> List[str]:
    filename = os.path.join(typeshed_location, "tests", "pytype_exclude_list.txt")
    skip_re = re.compile(r"^\s*([^\s#]+)\s*(?:#.*)?$")
    skip = []

    with open(filename) as f:
        for line in f:
            skip_match = skip_re.match(line)
            if skip_match:
                skip.append(skip_match.group(1))

    return skip


def run_pytype(*, filename: str, python_version: str, typeshed_location: str) -> Optional[str]:
    """Runs pytype, returning the stderr if any."""
    options = pytype_config.Options.create(
        filename, module_name=_get_module_name(filename), parse_pyi=True, python_version=python_version
    )
    old_typeshed_home = os.environ.get(TYPESHED_HOME, UNSET)
    os.environ[TYPESHED_HOME] = typeshed_location
    try:
        pytype_io.parse_pyi(options)
    except Exception:
        stderr = traceback.format_exc()
    else:
        stderr = None
    if old_typeshed_home is UNSET:
        del os.environ[TYPESHED_HOME]
    else:
        os.environ[TYPESHED_HOME] = old_typeshed_home
    return stderr


def _get_relative(filename: str) -> str:
    top = 0
    for d in TYPESHED_SUBDIRS:
        try:
            top = filename.index(d)
        except ValueError:
            continue
        else:
            break
    return filename[top:]


def _get_module_name(filename: str) -> str:
    """Converts a filename {subdir}/m.n/module/foo to module.foo."""
    return ".".join(_get_relative(filename).split(os.path.sep)[2:]).replace(".pyi", "").replace(".__init__", "")


def _is_version(path: str, version: str) -> bool:
    return any("{}{}{}".format(d, os.path.sep, version) in path for d in TYPESHED_SUBDIRS)


def check_subdirs_discoverable(subdir_paths: List[str]) -> None:
    for p in subdir_paths:
        if not os.path.isdir(p):
            raise SystemExit("Cannot find typeshed subdir at {} (specify parent dir via --typeshed-location)".format(p))


def determine_files_to_test(*, typeshed_location: str, paths: Sequence[str]) -> List[Tuple[str, int]]:
    """Determine all files to test, checking if it's in the exclude list and which Python versions to use.

    Returns a list of pairs of the file path and Python version as an int."""
    skipped = PathMatcher(load_exclude_list(typeshed_location))
    filenames = find_stubs_in_paths(paths)
    files = []
    for f in sorted(filenames):
        rel = _get_relative(f)
        if skipped.search(rel):
            continue
        if _is_version(f, "2and3"):
            files.append((f, 2))
            files.append((f, 3))
        elif _is_version(f, "2"):
            files.append((f, 2))
        elif _is_version(f, "3"):
            files.append((f, 3))
        else:
            print("Unrecognized path: {}".format(f))
    return files


def find_stubs_in_paths(paths: Sequence[str]) -> List[str]:
    filenames = []
    for path in paths:
        if os.path.isdir(path):
            for root, _, fns in os.walk(path):
                filenames.extend(os.path.join(root, fn) for fn in fns if fn.endswith(".pyi"))
        else:
            filenames.append(path)
    return filenames


def run_all_tests(*, files_to_test: Sequence[Tuple[str, int]], typeshed_location: str, print_stderr: bool, dry_run: bool) -> None:
    bad = []
    errors = 0
    total_tests = len(files_to_test)
    print("Testing files with pytype...")
    for i, (f, version) in enumerate(files_to_test):
        stderr = (
            run_pytype(
                filename=f,
                python_version="2.7" if version == 2 else "{0.major}.{0.minor}".format(sys.version_info),
                typeshed_location=typeshed_location,
            )
            if not dry_run
            else None
        )
        if stderr:
            if print_stderr:
                print(stderr)
            errors += 1
            stacktrace_final_line = stderr.rstrip().rsplit("\n", 1)[-1]
            bad.append((_get_relative(f), stacktrace_final_line))

        runs = i + 1
        if runs % 25 == 0:
            print("  {:3d}/{:d} with {:3d} errors".format(runs, total_tests, errors))

    print("Ran pytype with {:d} pyis, got {:d} errors.".format(total_tests, errors))
    for f, err in bad:
        print("{}: {}".format(f, err))
    if errors:
        raise SystemExit("\nRun again with --print-stderr to get the full stacktrace.")


if __name__ == "__main__":
    main()
