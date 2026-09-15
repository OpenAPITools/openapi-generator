#!/usr/bin/env python3
"""Select CI scopes from cumulative PR changes; uncertainty always runs the suite."""

import argparse
import copy
from collections import Counter
import itertools
import json
import os
from pathlib import Path
import re
import subprocess
import sys


GLOBAL_PATHS = (
    "CI/select_changes.py",
    "CI/change-scopes.json",
    "CI/tests/**",
    ".github/actions/compute-matrix/**",
    ".github/.test/ci-scopes.js",
    ".gitattributes",
    ".gitmodules",
)


class ConfigError(ValueError):
    """Invalid policy or invocation; unlike diff uncertainty this must fail CI."""


class DiffUncertain(Exception):
    """Comparison could not be verified safely."""


def validate_path(value, label, glob=False):
    if not isinstance(value, str) or not value or "\0" in value:
        raise ConfigError(f"{label}: expected a nonempty repository-relative path")
    if "\\" in value or value.startswith("/") or re.match(r"^[A-Za-z]:", value):
        raise ConfigError(f"{label}: use repository-relative POSIX paths")
    if any(part in ("", ".", "..") for part in value.rstrip("/").split("/")):
        raise ConfigError(f"{label}: invalid path components")
    if glob and ("[" in value or "]" in value):
        raise ConfigError(f"{label}: only *, ** and ? glob syntax is supported")
    return value


def validate_manifest(manifest):
    if not isinstance(manifest, dict) or type(manifest.get("version")) is not int or manifest["version"] != 1:
        raise ConfigError("manifest version must be 1")
    suites = manifest.get("suites")
    if not isinstance(suites, dict) or not suites:
        raise ConfigError("manifest suites must be a nonempty object")
    for suite_id, suite in suites.items():
        if not isinstance(suite_id, str) or not suite_id or not isinstance(suite, dict):
            raise ConfigError("each suite must have an ID and object definition")
        if "workflow" in suite:
            validate_path(suite["workflow"], f"{suite_id}.workflow")
        if "discovery" in suite and suite["discovery"] != "typescript":
            raise ConfigError(f"{suite_id}.discovery: only typescript discovery is supported")
        for field in ("paths", "shared_paths", "shadow_paths"):
            paths = suite.get(field, [])
            if not isinstance(paths, list):
                raise ConfigError(f"{suite_id}.{field}: expected an array")
            for path in paths:
                validate_path(path, f"{suite_id}.{field}", glob=True)
        matrix = suite.get("matrix", {})
        if not isinstance(matrix, dict):
            raise ConfigError(f"{suite_id}.matrix: expected an object")
        for axis, values in matrix.items():
            if not isinstance(axis, str) or not isinstance(values, list) or (not values and axis not in ("include", "exclude")):
                raise ConfigError(f"{suite_id}.matrix.{axis}: expected a nonempty array")
            if axis in ("include", "exclude"):
                if any(not isinstance(row, dict) or not row for row in values):
                    raise ConfigError(f"{suite_id}.matrix.{axis}: expected nonempty row objects")
                for row in values:
                    if "sample" in row:
                        validate_path(row["sample"], f"{suite_id}.matrix.{axis}.sample")
                    if axis == "exclude" and any(key not in matrix or key in ("include", "exclude") for key in row):
                        raise ConfigError(f"{suite_id}.matrix.exclude: unknown axis")
            elif axis == "sample":
                for sample in values:
                    validate_path(sample, f"{suite_id}.matrix.sample")
            elif any(isinstance(value, (list, dict)) or value is None for value in values):
                raise ConfigError(f"{suite_id}.matrix.{axis}: only scalar axis values are supported")
        if "exclude" in matrix and not any(axis not in ("include", "exclude") for axis in matrix):
            raise ConfigError(f"{suite_id}.matrix.exclude: requires matrix axes")
        if "discovery" in suite and ("sample" in matrix or any("sample" in row for row in matrix.get("include", []))):
            raise ConfigError(f"{suite_id}: discovery must not be combined with a sample matrix")
    return manifest


def load_manifest(path):
    try:
        with open(path, encoding="utf-8") as stream:
            return validate_manifest(json.load(stream))
    except (OSError, json.JSONDecodeError) as exc:
        raise ConfigError(f"cannot read manifest {path}: {exc}") from exc


def path_matches(path, pattern):
    """Git paths are literal; policy patterns support only *, ** and ?."""
    pattern = pattern.rstrip("/")
    if "*" not in pattern and "?" not in pattern:
        return path == pattern or path.startswith(pattern + "/")
    pieces = []
    index = 0
    while index < len(pattern):
        char = pattern[index]
        if pattern[index:index + 3] == "**/":
            pieces.append("(?:.*/)?")
            index += 3
        elif pattern[index:index + 2] == "**":
            pieces.append(".*")
            index += 2
        elif char == "*":
            pieces.append("[^/]*")
            index += 1
        elif char == "?":
            pieces.append("[^/]")
            index += 1
        else:
            pieces.append(re.escape(char))
            index += 1
    return re.fullmatch("".join(pieces), path, flags=re.DOTALL) is not None


def sample_matches(path, sample):
    sample = sample.rstrip("/")
    return path == sample or path.startswith(sample + "/")


def discover_typescript_samples(cwd="."):
    try:
        data = Git(cwd).run("ls-files", "-z", "--", "samples")
    except DiffUncertain as exc:
        raise ConfigError(f"cannot discover TypeScript sample ownership: {exc}") from exc
    tracked = {path.decode("utf-8", "surrogateescape") for path in data.split(b"\0") if path}
    roots = {
        path.rsplit("/", 1)[0] for path in tracked
        if path.endswith("/tsconfig.json")
    }
    return sorted(root for root in roots
                  if root + "/.openapi-generator-ignore" in tracked and root + "/package.json" in tracked)


def suite_samples(suite, cwd="."):
    if suite.get("discovery") == "typescript":
        return discover_typescript_samples(cwd)
    matrix = suite.get("matrix", {})
    samples = list(matrix.get("sample", []))
    for row in matrix.get("include", []):
        if "sample" in row and row["sample"] not in samples:
            samples.append(row["sample"])
    return samples


def expand_matrix(matrix):
    """Expand GitHub's Cartesian axes, exclusions, then compatible include rows."""
    axes = [axis for axis in matrix if axis not in ("include", "exclude")]
    originals = [dict(zip(axes, values)) for values in itertools.product(*(matrix[axis] for axis in axes))] if axes else []
    originals = [
        row for row in originals
        if not any(all(key in row and row[key] == value for key, value in excluded.items())
                   for excluded in matrix.get("exclude", []))
    ]
    combinations = copy.deepcopy(originals)
    additions = []
    for included in matrix.get("include", []):
        compatible = False
        for index, original in enumerate(originals):
            if all(key not in original or original[key] == value for key, value in included.items()):
                combinations[index].update(included)
                compatible = True
        if not compatible:
            additions.append(copy.deepcopy(included))
    return combinations + additions


def combination_counts(rows):
    return Counter(json.dumps(row, sort_keys=True, ensure_ascii=True) for row in rows)


def full_selection(suite, reasons, samples=None):
    return {
        "matrix": copy.deepcopy(suite.get("matrix", {})),
        "samples": suite_samples(suite) if samples is None else list(samples),
        "has_changes": True,
        "run_all": True,
        "reasons": list(reasons),
    }


def select_changes(manifest, suite_id, changed_paths=None, full_reason=None, mode="enforce", cwd="."):
    validate_manifest(manifest)
    if suite_id not in manifest["suites"]:
        raise ConfigError(f"unknown suite: {suite_id}")
    if mode not in ("enforce", "shadow"):
        raise ConfigError(f"unknown selection mode: {mode}")
    suite = manifest["suites"][suite_id]
    all_samples = suite_samples(suite, cwd)
    if full_reason is not None:
        proposed = full_selection(suite, [full_reason], all_samples)
    elif changed_paths is None:
        raise ConfigError("selection requires verified changes or a full-run reason")
    else:
        changes = sorted(set(changed_paths))
        full_matches = [
            path for path in changes
            if path == suite.get("workflow")
            or any(path_matches(path, pattern) for pattern in GLOBAL_PATHS)
            or any(path_matches(path, pattern) for pattern in suite.get("shared_paths", []))
            or any(path_matches(path, pattern) for pattern in suite.get("paths", []))
        ]
        if full_matches:
            proposed = full_selection(suite, ["full scope input changed: " + path for path in full_matches], all_samples)
        else:
            samples = [
                sample for sample in all_samples
                if any(sample_matches(path, sample) for path in changes)
            ]
            matrix = copy.deepcopy(suite.get("matrix", {}))
            if not samples:
                matrix = {"include": []}
            elif not suite.get("discovery"):
                selected = {sample.rstrip("/") for sample in samples}
                if "sample" in matrix:
                    matrix["sample"] = [sample for sample in matrix["sample"] if sample.rstrip("/") in selected]
                    # Include-only samples cannot coexist with an empty Cartesian axis.
                    if not matrix["sample"]:
                        matrix = {"include": [
                            row for row in matrix.get("include", [])
                            if row.get("sample", "").rstrip("/") in selected
                        ]}
                for field in ("include", "exclude"):
                    if field in matrix:
                        matrix[field] = [
                            row for row in matrix[field]
                            if "sample" not in row or row["sample"].rstrip("/") in selected
                        ]
            proposed = {
                "matrix": matrix,
                "samples": samples,
                "has_changes": bool(samples),
                "run_all": False,
                "reasons": ["selected sample: " + sample for sample in samples] or ["no owned inputs changed"],
            }
            if samples and not suite.get("discovery"):
                original_rows = expand_matrix(suite.get("matrix", {}))
                expected = [
                    row for row in original_rows
                    if "sample" in row and row["sample"].rstrip("/") in selected
                ]
                if combination_counts(expand_matrix(matrix)) != combination_counts(expected):
                    proposed = full_selection(suite, ["matrix includes cannot be filtered without changing original combinations"], all_samples)
                elif not expected:
                    proposed.update(matrix={"include": []}, samples=[], has_changes=False,
                                    reasons=["selected samples have no runnable matrix combinations"])
    if mode == "shadow":
        historical_trigger = (
            "shadow_paths" not in suite
            or proposed["has_changes"]
            or any(path_matches(path, pattern) for path in changes for pattern in suite["shadow_paths"])
        )
        if historical_trigger:
            result = full_selection(suite, ["shadow mode: executing full suite"] + proposed["reasons"], all_samples)
        else:
            result = copy.deepcopy(proposed)
            result["reasons"] = ["shadow mode: no owned input or historical workflow trigger changed"] + proposed["reasons"]
        result["proposed_selection"] = proposed
        return result
    return proposed


class Git:
    def __init__(self, cwd="."):
        self.cwd = cwd

    def run(self, *args):
        try:
            process = subprocess.run(
                ["git", *args], cwd=self.cwd, stdout=subprocess.PIPE,
                stderr=subprocess.PIPE, timeout=120, check=False,
            )
        except (OSError, subprocess.TimeoutExpired) as exc:
            raise DiffUncertain(f"git command unavailable: {exc}") from exc
        if process.returncode:
            detail = process.stderr.decode("utf-8", "replace").strip()
            raise DiffUncertain(f"git {args[0]} failed: {detail}")
        return process.stdout

    def text(self, *args):
        return self.run(*args).decode("utf-8", "replace").strip()

    def ensure_commit(self, sha):
        if not isinstance(sha, str) or not re.fullmatch(r"[0-9a-fA-F]{40}|[0-9a-fA-F]{64}", sha):
            raise DiffUncertain("missing or invalid full commit SHA")
        sha = sha.lower()
        try:
            resolved = self.text("rev-parse", "--verify", sha + "^{commit}")
        except DiffUncertain:
            self.run("fetch", "--no-tags", "--depth=1", "origin", sha)
            resolved = self.text("rev-parse", "--verify", sha + "^{commit}")
        if resolved != sha:
            raise DiffUncertain("commit SHA did not resolve exactly")
        return sha

    def merge_base(self, base, head):
        def resolve():
            candidates = self.text("merge-base", "--all", base, head).splitlines()
            if len(candidates) != 1:
                raise DiffUncertain("PR does not have a unique verified merge base")
            return candidates[0]

        try:
            return resolve()
        except DiffUncertain:
            if self.text("rev-parse", "--is-shallow-repository") != "true":
                raise
        for depth in (64, 256, 1024):
            self.run("fetch", "--no-tags", f"--deepen={depth}", "origin", base, head)
            try:
                return resolve()
            except DiffUncertain:
                continue
        raise DiffUncertain("merge base unavailable after bounded history deepening")

    def diff(self, base, head):
        data = self.run("diff", "--name-only", "--no-renames", "-z", base, head, "--")
        if data and not data.endswith(b"\0"):
            raise DiffUncertain("git diff returned malformed NUL-delimited paths")
        return {path.decode("utf-8", "surrogateescape") for path in data.split(b"\0") if path}


def pr_changes(git, base, head, checkout_sha=None, allow_merge=False):
    base = git.ensure_commit(base)
    head = git.ensure_commit(head)
    checkout = git.text("rev-parse", "--verify", "HEAD")
    if checkout_sha is not None and checkout != checkout_sha.lower():
        raise DiffUncertain("checkout does not match the event SHA")
    merge_parent = None
    if checkout != head:
        if not allow_merge:
            raise DiffUncertain("checkout does not match the verified PR head")
        # Read the commit object, not rev-list: shallow boundaries hide its parents.
        parents = [
            line.split()[1] for line in git.text("cat-file", "-p", checkout).split("\n\n", 1)[0].splitlines()
            if line.startswith("parent ")
        ]
        if parents != [base, head]:
            raise DiffUncertain("checkout is not the verified PR merge (base, head)")
        merge_parent = base
    merge_base = git.merge_base(base, head)
    paths = git.diff(merge_base, head)
    if merge_parent is not None:
        paths.update(git.diff(merge_parent, checkout))
    return sorted(paths)


def env_bool(value, name):
    if value is None or value == "":
        return False
    if value.lower() not in ("true", "false", "1", "0"):
        raise ConfigError(f"{name} must be true or false")
    return value.lower() in ("true", "1")


def resolve_changes(provider, env=None, cwd="."):
    env = os.environ if env is None else env
    git = Git(cwd)
    if provider == "github":
        if env.get("GITHUB_EVENT_NAME") != "pull_request":
            return None, "non-PR GitHub event: full suite"
        try:
            with open(env.get("GITHUB_EVENT_PATH", ""), encoding="utf-8") as stream:
                event = json.load(stream)
            pr = event["pull_request"]
            base, head = pr["base"]["sha"], pr["head"]["sha"]
            checkout_sha = env.get("GITHUB_SHA")
            if not checkout_sha:
                raise DiffUncertain("missing GitHub event checkout SHA")
        except (OSError, ValueError, KeyError, TypeError) as exc:
            return None, f"diff uncertainty: unavailable PR event metadata: {exc}"
        allow_merge = True
    elif provider == "circleci":
        if not env_bool(env.get("CI_IS_PR"), "CI_IS_PR"):
            return None, "non-PR or unverified CircleCI event: full suite"
        base, head = env.get("CI_BASE_SHA"), env.get("CI_HEAD_SHA")
        checkout_sha = env.get("CIRCLE_SHA1") or head
        allow_merge = False
    else:
        raise ConfigError(f"unknown provider: {provider}")
    try:
        return pr_changes(git, base, head, checkout_sha, allow_merge), None
    except DiffUncertain as exc:
        return None, f"diff uncertainty: {exc}"


def emit_outputs(result, output=None, summary=None):
    encoded = json.dumps(result, ensure_ascii=True, separators=(",", ":"))
    print(encoded)
    if output:
        with open(output, "a", encoding="utf-8") as stream:
            for key, value in result.items():
                stream.write(key + "=" + json.dumps(value, ensure_ascii=True, separators=(",", ":")) + "\n")
    if summary:
        with open(summary, "a", encoding="utf-8") as stream:
            stream.write("\n### CI change selection\n\n```json\n" + encoded + "\n```\n")


def validate_targets(result, cwd="."):
    missing = [sample for sample in result["samples"] if not (Path(cwd) / sample).is_dir()]
    if missing:
        raise ConfigError("scheduled sample directories do not exist; update ownership: " + ", ".join(missing))


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--suite", required=True)
    parser.add_argument("--manifest", default="CI/change-scopes.json")
    parser.add_argument("--provider", choices=("github", "circleci"))
    parser.add_argument("--mode", choices=("enforce", "shadow"), default="enforce")
    parser.add_argument("--force-full", action="store_true")
    parser.add_argument("--output", default=os.environ.get("GITHUB_OUTPUT"))
    args = parser.parse_args(argv)
    try:
        manifest = load_manifest(args.manifest)
        if args.suite not in manifest["suites"]:
            raise ConfigError(f"unknown suite: {args.suite}")
        provider = args.provider or ("circleci" if os.environ.get("CIRCLECI") == "true" else "github")
        force_full = args.force_full or env_bool(os.environ.get("CI_FORCE_FULL"), "CI_FORCE_FULL")
        paths, reason = (None, "explicit full-run override") if force_full else resolve_changes(provider)
        result = select_changes(manifest, args.suite, paths, reason, args.mode)
        validate_targets(result)
        emit_outputs(result, args.output, os.environ.get("GITHUB_STEP_SUMMARY"))
    except (ConfigError, OSError) as exc:
        print(f"CI selector error: {exc}", file=sys.stderr)
        return 2
    return 0


if __name__ == "__main__":
    sys.exit(main())
