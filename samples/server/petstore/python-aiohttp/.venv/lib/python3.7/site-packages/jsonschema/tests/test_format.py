"""
Tests for the parts of jsonschema related to the :validator:`format` property.

"""

from jsonschema.tests.compat import mock, unittest

from jsonschema import FormatError, ValidationError, FormatChecker
from jsonschema.validators import Draft4Validator


class TestFormatChecker(unittest.TestCase):
    def setUp(self):
        self.fn = mock.Mock()

    def test_it_can_validate_no_formats(self):
        checker = FormatChecker(formats=())
        self.assertFalse(checker.checkers)

    def test_it_raises_a_key_error_for_unknown_formats(self):
        with self.assertRaises(KeyError):
            FormatChecker(formats=["o noes"])

    def test_it_can_register_cls_checkers(self):
        with mock.patch.dict(FormatChecker.checkers, clear=True):
            FormatChecker.cls_checks("new")(self.fn)
            self.assertEqual(FormatChecker.checkers, {"new": (self.fn, ())})

    def test_it_can_register_checkers(self):
        checker = FormatChecker()
        checker.checks("new")(self.fn)
        self.assertEqual(
            checker.checkers,
            dict(FormatChecker.checkers, new=(self.fn, ()))
        )

    def test_it_catches_registered_errors(self):
        checker = FormatChecker()
        cause = self.fn.side_effect = ValueError()

        checker.checks("foo", raises=ValueError)(self.fn)

        with self.assertRaises(FormatError) as cm:
            checker.check("bar", "foo")

        self.assertIs(cm.exception.cause, cause)
        self.assertIs(cm.exception.__cause__, cause)

        # Unregistered errors should not be caught
        self.fn.side_effect = AttributeError
        with self.assertRaises(AttributeError):
            checker.check("bar", "foo")

    def test_format_error_causes_become_validation_error_causes(self):
        checker = FormatChecker()
        checker.checks("foo", raises=ValueError)(self.fn)
        cause = self.fn.side_effect = ValueError()
        validator = Draft4Validator({"format": "foo"}, format_checker=checker)

        with self.assertRaises(ValidationError) as cm:
            validator.validate("bar")

        self.assertIs(cm.exception.__cause__, cause)
