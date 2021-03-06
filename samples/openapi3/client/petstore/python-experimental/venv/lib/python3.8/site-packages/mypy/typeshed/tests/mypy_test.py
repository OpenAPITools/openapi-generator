#!/usr/bin/env python3
"""Test runner for typeshed.

Depends on mypy being installed.

Approach:

1. Parse sys.argv
2. Compute appropriate arguments for mypy
3. Stuff those arguments into sys.argv
4. Run mypy.main('')
5. Repeat steps 2-4 for other mypy runs (e.g. --py2)
"""

import argparse
import os
import re
import sys

parser = argparse.ArgumentParser(description="Test runner for typeshed. " "Patterns are unanchored regexps on the full path.")
parser.add_argument("-v", "--verbose", action="count", default=0, help="More output")
parser.add_argument("-n", "--dry-run", action="store_true", help="Don't actually run mypy")
parser.add_argument("-x", "--exclude", type=str, nargs="*", help="Exclude pattern")
parser.add_argument("-p", "--python-version", type=str, nargs="*", help="These versions only (major[.minor])")
parser.add_argument("--platform", help="Run mypy for a certain OS platform (defaults to sys.platform)")
parser.add_argument(
    "--warn-unused-ignores",
    action="store_true",
    help="Run mypy with --warn-unused-ignores "
    "(hint: only get rid of warnings that are "
    "unused for all platforms and Python versions)",
)

parser.add_argument("filter", type=str, nargs="*", help="Include pattern (default all)")


def log(args, *varargs):
    if args.verbose >= 2:
        print(*varargs)


def match(fn, args, exclude_list):
    if exclude_list.match(fn):
        log(args, fn, "exluded by exclude list")
        return False
    if not args.filter and not args.exclude:
        log(args, fn, "accept by default")
        return True
    if args.exclude:
        for f in args.exclude:
            if re.search(f, fn):
                log(args, fn, "excluded by pattern", f)
                return False
    if args.filter:
        for f in args.filter:
            if re.search(f, fn):
                log(args, fn, "accepted by pattern", f)
                return True
    if args.filter:
        log(args, fn, "rejected (no pattern matches)")
        return False
    log(args, fn, "accepted (no exclude pattern matches)")
    return True


def libpath(major, minor):
    versions = ["%d.%d" % (major, minor) for minor in reversed(range(minor + 1))]
    versions.append(str(major))
    versions.append("2and3")
    paths = []
    for v in versions:
        for top in ["stdlib", "third_party"]:
            p = os.path.join(top, v)
            if os.path.isdir(p):
                paths.append(p)
    return paths


def main():
    args = parser.parse_args()

    with open(os.path.join(os.path.dirname(__file__), "mypy_exclude_list.txt")) as f:
        exclude_list = re.compile("(%s)$" % "|".join(re.findall(r"^\s*([^\s#]+)\s*(?:#.*)?$", f.read(), flags=re.M)))

    try:
        from mypy.main import main as mypy_main
    except ImportError:
        print("Cannot import mypy. Did you install it?")
        sys.exit(1)

    versions = [(3, 9), (3, 8), (3, 7), (3, 6), (3, 5), (2, 7)]
    if args.python_version:
        versions = [v for v in versions if any(("%d.%d" % v).startswith(av) for av in args.python_version)]
        if not versions:
            print("--- no versions selected ---")
            sys.exit(1)

    code = 0
    runs = 0
    for major, minor in versions:
        roots = libpath(major, minor)
        files = []
        seen = {"__builtin__", "builtins", "typing"}  # Always ignore these.
        for root in roots:
            names = os.listdir(root)
            for name in names:
                full = os.path.join(root, name)
                mod, ext = os.path.splitext(name)
                if mod in seen or mod.startswith("."):
                    continue
                if ext in [".pyi", ".py"]:
                    if match(full, args, exclude_list):
                        seen.add(mod)
                        files.append(full)
                elif os.path.isfile(os.path.join(full, "__init__.pyi")) or os.path.isfile(os.path.join(full, "__init__.py")):
                    for r, ds, fs in os.walk(full):
                        ds.sort()
                        fs.sort()
                        for f in fs:
                            m, x = os.path.splitext(f)
                            if x in [".pyi", ".py"]:
                                fn = os.path.join(r, f)
                                if match(fn, args, exclude_list):
                                    seen.add(mod)
                                    files.append(fn)
        if files:
            runs += 1
            flags = ["--python-version", "%d.%d" % (major, minor)]
            flags.append("--strict-optional")
            flags.append("--no-site-packages")
            flags.append("--show-traceback")
            flags.append("--no-implicit-optional")
            flags.append("--disallow-any-generics")
            flags.append("--disallow-subclassing-any")
            # Setting custom typeshed dir prevents mypy from falling back to its bundled typeshed in
            # case of stub deletions
            flags.append("--custom-typeshed-dir")
            flags.append(os.path.dirname(os.path.dirname(__file__)))
            if args.warn_unused_ignores:
                flags.append("--warn-unused-ignores")
            if args.platform:
                flags.extend(["--platform", args.platform])
            sys.argv = ["mypy"] + flags + files
            if args.verbose:
                print("running", " ".join(sys.argv))
            else:
                print("running mypy", " ".join(flags), "# with", len(files), "files")
            try:
                if not args.dry_run:
                    mypy_main("", sys.stdout, sys.stderr)
            except SystemExit as err:
                code = max(code, err.code)
    if code:
        print("--- exit status", code, "---")
        sys.exit(code)
    if not runs:
        print("--- nothing to do; exit 1 ---")
        sys.exit(1)


if __name__ == "__main__":
    main()
