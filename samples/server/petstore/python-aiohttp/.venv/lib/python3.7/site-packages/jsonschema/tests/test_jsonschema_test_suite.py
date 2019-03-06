"""
Test runner for the JSON Schema official test suite

Tests comprehensive correctness of each draft's validator.

See https://github.com/json-schema/JSON-Schema-Test-Suite for details.

"""

from contextlib import closing
from decimal import Decimal
import glob
import json
import io
import itertools
import os
import re
import subprocess
import sys

try:
    from sys import pypy_version_info
except ImportError:
    pypy_version_info = None

from jsonschema import (
    FormatError, SchemaError, ValidationError, Draft3Validator,
    Draft4Validator, FormatChecker, draft3_format_checker,
    draft4_format_checker, validate,
)
from jsonschema.compat import PY3
from jsonschema.tests.compat import mock, unittest
import jsonschema


REPO_ROOT = os.path.join(os.path.dirname(jsonschema.__file__), os.path.pardir)
SUITE = os.getenv("JSON_SCHEMA_TEST_SUITE", os.path.join(REPO_ROOT, "json"))

if not os.path.isdir(SUITE):
    raise ValueError(
        "Can't find the JSON-Schema-Test-Suite directory. Set the "
        "'JSON_SCHEMA_TEST_SUITE' environment variable or run the tests from "
        "alongside a checkout of the suite."
    )

TESTS_DIR = os.path.join(SUITE, "tests")
JSONSCHEMA_SUITE = os.path.join(SUITE, "bin", "jsonschema_suite")

remotes_stdout = subprocess.Popen(
    ["python", JSONSCHEMA_SUITE, "remotes"], stdout=subprocess.PIPE,
).stdout

with closing(remotes_stdout):
    if PY3:
        remotes_stdout = io.TextIOWrapper(remotes_stdout)
    REMOTES = json.load(remotes_stdout)


def make_case(schema, data, valid, name):
    if valid:
        def test_case(self):
            kwargs = getattr(self, "validator_kwargs", {})
            validate(data, schema, cls=self.validator_class, **kwargs)
    else:
        def test_case(self):
            kwargs = getattr(self, "validator_kwargs", {})
            with self.assertRaises(ValidationError):
                validate(data, schema, cls=self.validator_class, **kwargs)

    if not PY3:
        name = name.encode("utf-8")
    test_case.__name__ = name

    return test_case


def maybe_skip(skip, test_case, case, test):
    if skip is not None:
        reason = skip(case, test)
        if reason is not None:
            test_case = unittest.skip(reason)(test_case)
    return test_case


def load_json_cases(tests_glob, ignore_glob="", basedir=TESTS_DIR, skip=None):
    if ignore_glob:
        ignore_glob = os.path.join(basedir, ignore_glob)

    def add_test_methods(test_class):
        ignored = set(glob.iglob(ignore_glob))

        for filename in glob.iglob(os.path.join(basedir, tests_glob)):
            if filename in ignored:
                continue

            validating, _ = os.path.splitext(os.path.basename(filename))
            id = itertools.count(1)

            with open(filename) as test_file:
                for case in json.load(test_file):
                    for test in case["tests"]:
                        name = "test_%s_%s_%s" % (
                            validating,
                            next(id),
                            re.sub(r"[\W ]+", "_", test["description"]),
                        )
                        assert not hasattr(test_class, name), name

                        test_case = make_case(
                            data=test["data"],
                            schema=case["schema"],
                            valid=test["valid"],
                            name=name,
                        )
                        test_case = maybe_skip(skip, test_case, case, test)
                        setattr(test_class, name, test_case)

        return test_class
    return add_test_methods


class TypesMixin(object):
    @unittest.skipIf(PY3, "In Python 3 json.load always produces unicode")
    def test_string_a_bytestring_is_a_string(self):
        self.validator_class({"type": "string"}).validate(b"foo")


class DecimalMixin(object):
    def test_it_can_validate_with_decimals(self):
        schema = {"type": "number"}
        validator = self.validator_class(
            schema, types={"number": (int, float, Decimal)}
        )

        for valid in [1, 1.1, Decimal(1) / Decimal(8)]:
            validator.validate(valid)

        for invalid in ["foo", {}, [], True, None]:
            with self.assertRaises(ValidationError):
                validator.validate(invalid)


def missing_format(checker):
    def missing_format(case, test):
        format = case["schema"].get("format")
        if format not in checker.checkers:
            return "Format checker {0!r} not found.".format(format)
        elif (
            format == "date-time" and
            pypy_version_info is not None and
            pypy_version_info[:2] <= (1, 9)
        ):
            # datetime.datetime is overzealous about typechecking in <=1.9
            return "datetime.datetime is broken on this version of PyPy."
    return missing_format


class FormatMixin(object):
    def test_it_returns_true_for_formats_it_does_not_know_about(self):
        validator = self.validator_class(
            {"format": "carrot"}, format_checker=FormatChecker(),
        )
        validator.validate("bugs")

    def test_it_does_not_validate_formats_by_default(self):
        validator = self.validator_class({})
        self.assertIsNone(validator.format_checker)

    def test_it_validates_formats_if_a_checker_is_provided(self):
        checker = mock.Mock(spec=FormatChecker)
        validator = self.validator_class(
            {"format": "foo"}, format_checker=checker,
        )

        validator.validate("bar")

        checker.check.assert_called_once_with("bar", "foo")

        cause = ValueError()
        checker.check.side_effect = FormatError('aoeu', cause=cause)

        with self.assertRaises(ValidationError) as cm:
            validator.validate("bar")
        # Make sure original cause is attached
        self.assertIs(cm.exception.cause, cause)

    def test_it_validates_formats_of_any_type(self):
        checker = mock.Mock(spec=FormatChecker)
        validator = self.validator_class(
            {"format": "foo"}, format_checker=checker,
        )

        validator.validate([1, 2, 3])

        checker.check.assert_called_once_with([1, 2, 3], "foo")

        cause = ValueError()
        checker.check.side_effect = FormatError('aoeu', cause=cause)

        with self.assertRaises(ValidationError) as cm:
            validator.validate([1, 2, 3])
        # Make sure original cause is attached
        self.assertIs(cm.exception.cause, cause)


if sys.maxunicode == 2 ** 16 - 1:          # This is a narrow build.
    def narrow_unicode_build(case, test):
        if "supplementary Unicode" in test["description"]:
            return "Not running surrogate Unicode case, this Python is narrow."
else:
    def narrow_unicode_build(case, test):  # This isn't, skip nothing.
        return


@load_json_cases(
    "draft3/*.json",
    skip=narrow_unicode_build,
    ignore_glob="draft3/refRemote.json",
)
@load_json_cases(
    "draft3/optional/format.json", skip=missing_format(draft3_format_checker)
)
@load_json_cases("draft3/optional/bignum.json")
@load_json_cases("draft3/optional/zeroTerminatedFloats.json")
class TestDraft3(unittest.TestCase, TypesMixin, DecimalMixin, FormatMixin):
    validator_class = Draft3Validator
    validator_kwargs = {"format_checker": draft3_format_checker}

    def test_any_type_is_valid_for_type_any(self):
        validator = self.validator_class({"type": "any"})
        validator.validate(mock.Mock())

    # TODO: we're in need of more meta schema tests
    def test_invalid_properties(self):
        with self.assertRaises(SchemaError):
            validate({}, {"properties": {"test": True}},
                     cls=self.validator_class)

    def test_minItems_invalid_string(self):
        with self.assertRaises(SchemaError):
            # needs to be an integer
            validate([1], {"minItems": "1"}, cls=self.validator_class)


@load_json_cases(
    "draft4/*.json",
    skip=narrow_unicode_build,
    ignore_glob="draft4/refRemote.json",
)
@load_json_cases(
    "draft4/optional/format.json", skip=missing_format(draft4_format_checker)
)
@load_json_cases("draft4/optional/bignum.json")
@load_json_cases("draft4/optional/zeroTerminatedFloats.json")
class TestDraft4(unittest.TestCase, TypesMixin, DecimalMixin, FormatMixin):
    validator_class = Draft4Validator
    validator_kwargs = {"format_checker": draft4_format_checker}

    # TODO: we're in need of more meta schema tests
    def test_invalid_properties(self):
        with self.assertRaises(SchemaError):
            validate({}, {"properties": {"test": True}},
                     cls=self.validator_class)

    def test_minItems_invalid_string(self):
        with self.assertRaises(SchemaError):
            # needs to be an integer
            validate([1], {"minItems": "1"}, cls=self.validator_class)


class RemoteRefResolutionMixin(object):
    def setUp(self):
        patch = mock.patch("jsonschema.validators.requests")
        requests = patch.start()
        requests.get.side_effect = self.resolve
        self.addCleanup(patch.stop)

    def resolve(self, reference):
        _, _, reference = reference.partition("http://localhost:1234/")
        return mock.Mock(**{"json.return_value": REMOTES.get(reference)})


@load_json_cases("draft3/refRemote.json")
class Draft3RemoteResolution(RemoteRefResolutionMixin, unittest.TestCase):
    validator_class = Draft3Validator


@load_json_cases("draft4/refRemote.json")
class Draft4RemoteResolution(RemoteRefResolutionMixin, unittest.TestCase):
    validator_class = Draft4Validator
