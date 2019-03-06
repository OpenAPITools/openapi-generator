from collections import deque
from contextlib import contextmanager
import json

from jsonschema import FormatChecker, ValidationError
from jsonschema.tests.compat import mock, unittest
from jsonschema.validators import (
    RefResolutionError, UnknownType, Draft3Validator,
    Draft4Validator, RefResolver, create, extend, validator_for, validate,
)


class TestCreateAndExtend(unittest.TestCase):
    def setUp(self):
        self.meta_schema = {u"properties": {u"smelly": {}}}
        self.smelly = mock.MagicMock()
        self.validators = {u"smelly": self.smelly}
        self.types = {u"dict": dict}
        self.Validator = create(
            meta_schema=self.meta_schema,
            validators=self.validators,
            default_types=self.types,
        )

        self.validator_value = 12
        self.schema = {u"smelly": self.validator_value}
        self.validator = self.Validator(self.schema)

    def test_attrs(self):
        self.assertEqual(self.Validator.VALIDATORS, self.validators)
        self.assertEqual(self.Validator.META_SCHEMA, self.meta_schema)
        self.assertEqual(self.Validator.DEFAULT_TYPES, self.types)

    def test_init(self):
        self.assertEqual(self.validator.schema, self.schema)

    def test_iter_errors(self):
        instance = "hello"

        self.smelly.return_value = []
        self.assertEqual(list(self.validator.iter_errors(instance)), [])

        error = mock.Mock()
        self.smelly.return_value = [error]
        self.assertEqual(list(self.validator.iter_errors(instance)), [error])

        self.smelly.assert_called_with(
            self.validator, self.validator_value, instance, self.schema,
        )

    def test_if_a_version_is_provided_it_is_registered(self):
        with mock.patch("jsonschema.validators.validates") as validates:
            validates.side_effect = lambda version: lambda cls: cls
            Validator = create(meta_schema={u"id": ""}, version="my version")
        validates.assert_called_once_with("my version")
        self.assertEqual(Validator.__name__, "MyVersionValidator")

    def test_if_a_version_is_not_provided_it_is_not_registered(self):
        with mock.patch("jsonschema.validators.validates") as validates:
            create(meta_schema={u"id": "id"})
        self.assertFalse(validates.called)

    def test_extend(self):
        validators = dict(self.Validator.VALIDATORS)
        new = mock.Mock()

        Extended = extend(self.Validator, validators={u"a new one": new})

        validators.update([(u"a new one", new)])
        self.assertEqual(Extended.VALIDATORS, validators)
        self.assertNotIn(u"a new one", self.Validator.VALIDATORS)

        self.assertEqual(Extended.META_SCHEMA, self.Validator.META_SCHEMA)
        self.assertEqual(Extended.DEFAULT_TYPES, self.Validator.DEFAULT_TYPES)


class TestIterErrors(unittest.TestCase):
    def setUp(self):
        self.validator = Draft3Validator({})

    def test_iter_errors(self):
        instance = [1, 2]
        schema = {
            u"disallow": u"array",
            u"enum": [["a", "b", "c"], ["d", "e", "f"]],
            u"minItems": 3,
        }

        got = (e.message for e in self.validator.iter_errors(instance, schema))
        expected = [
            "%r is disallowed for [1, 2]" % (schema["disallow"],),
            "[1, 2] is too short",
            "[1, 2] is not one of %r" % (schema["enum"],),
        ]
        self.assertEqual(sorted(got), sorted(expected))

    def test_iter_errors_multiple_failures_one_validator(self):
        instance = {"foo": 2, "bar": [1], "baz": 15, "quux": "spam"}
        schema = {
            u"properties": {
                "foo": {u"type": "string"},
                "bar": {u"minItems": 2},
                "baz": {u"maximum": 10, u"enum": [2, 4, 6, 8]},
            },
        }

        errors = list(self.validator.iter_errors(instance, schema))
        self.assertEqual(len(errors), 4)


class TestValidationErrorMessages(unittest.TestCase):
    def message_for(self, instance, schema, *args, **kwargs):
        kwargs.setdefault("cls", Draft3Validator)
        with self.assertRaises(ValidationError) as e:
            validate(instance, schema, *args, **kwargs)
        return e.exception.message

    def test_single_type_failure(self):
        message = self.message_for(instance=1, schema={u"type": u"string"})
        self.assertEqual(message, "1 is not of type %r" % u"string")

    def test_single_type_list_failure(self):
        message = self.message_for(instance=1, schema={u"type": [u"string"]})
        self.assertEqual(message, "1 is not of type %r" % u"string")

    def test_multiple_type_failure(self):
        types = u"string", u"object"
        message = self.message_for(instance=1, schema={u"type": list(types)})
        self.assertEqual(message, "1 is not of type %r, %r" % types)

    def test_object_without_title_type_failure(self):
        type = {u"type": [{u"minimum": 3}]}
        message = self.message_for(instance=1, schema={u"type": [type]})
        self.assertEqual(message, "1 is not of type %r" % (type,))

    def test_object_with_name_type_failure(self):
        name = "Foo"
        schema = {u"type": [{u"name": name, u"minimum": 3}]}
        message = self.message_for(instance=1, schema=schema)
        self.assertEqual(message, "1 is not of type %r" % (name,))

    def test_minimum(self):
        message = self.message_for(instance=1, schema={"minimum": 2})
        self.assertEqual(message, "1 is less than the minimum of 2")

    def test_maximum(self):
        message = self.message_for(instance=1, schema={"maximum": 0})
        self.assertEqual(message, "1 is greater than the maximum of 0")

    def test_dependencies_failure_has_single_element_not_list(self):
        depend, on = "bar", "foo"
        schema = {u"dependencies": {depend: on}}
        message = self.message_for({"bar": 2}, schema)
        self.assertEqual(message, "%r is a dependency of %r" % (on, depend))

    def test_additionalItems_single_failure(self):
        message = self.message_for(
            [2], {u"items": [], u"additionalItems": False},
        )
        self.assertIn("(2 was unexpected)", message)

    def test_additionalItems_multiple_failures(self):
        message = self.message_for(
            [1, 2, 3], {u"items": [], u"additionalItems": False}
        )
        self.assertIn("(1, 2, 3 were unexpected)", message)

    def test_additionalProperties_single_failure(self):
        additional = "foo"
        schema = {u"additionalProperties": False}
        message = self.message_for({additional: 2}, schema)
        self.assertIn("(%r was unexpected)" % (additional,), message)

    def test_additionalProperties_multiple_failures(self):
        schema = {u"additionalProperties": False}
        message = self.message_for(dict.fromkeys(["foo", "bar"]), schema)

        self.assertIn(repr("foo"), message)
        self.assertIn(repr("bar"), message)
        self.assertIn("were unexpected)", message)

    def test_invalid_format_default_message(self):
        checker = FormatChecker(formats=())
        check_fn = mock.Mock(return_value=False)
        checker.checks(u"thing")(check_fn)

        schema = {u"format": u"thing"}
        message = self.message_for("bla", schema, format_checker=checker)

        self.assertIn(repr("bla"), message)
        self.assertIn(repr("thing"), message)
        self.assertIn("is not a", message)

    def test_additionalProperties_false_patternProperties(self):
        schema = {u"type": u"object",
                  u"additionalProperties": False,
                  u"patternProperties": {
                      u"^abc$": {u"type": u"string"},
                      u"^def$": {u"type": u"string"}
                  }}
        message = self.message_for({u"zebra": 123}, schema,
                                   cls=Draft4Validator)
        self.assertEqual(
            message,
            "{} does not match any of the regexes: {}, {}".format(
                repr(u"zebra"), repr(u"^abc$"), repr(u"^def$"),
            ),
        )
        message = self.message_for({u"zebra": 123, u"fish": 456}, schema,
                                   cls=Draft4Validator)
        self.assertEqual(
            message,
            "{}, {} do not match any of the regexes: {}, {}".format(
                repr(u"fish"), repr(u"zebra"), repr(u"^abc$"), repr(u"^def$")
            ),
        )


class TestValidationErrorDetails(unittest.TestCase):
    # TODO: These really need unit tests for each individual validator, rather
    #       than just these higher level tests.
    def test_anyOf(self):
        instance = 5
        schema = {
            "anyOf": [
                {"minimum": 20},
                {"type": "string"},
            ],
        }

        validator = Draft4Validator(schema)
        errors = list(validator.iter_errors(instance))
        self.assertEqual(len(errors), 1)
        e = errors[0]

        self.assertEqual(e.validator, "anyOf")
        self.assertEqual(e.validator_value, schema["anyOf"])
        self.assertEqual(e.instance, instance)
        self.assertEqual(e.schema, schema)
        self.assertIsNone(e.parent)

        self.assertEqual(e.path, deque([]))
        self.assertEqual(e.relative_path, deque([]))
        self.assertEqual(e.absolute_path, deque([]))

        self.assertEqual(e.schema_path, deque(["anyOf"]))
        self.assertEqual(e.relative_schema_path, deque(["anyOf"]))
        self.assertEqual(e.absolute_schema_path, deque(["anyOf"]))

        self.assertEqual(len(e.context), 2)

        e1, e2 = sorted_errors(e.context)

        self.assertEqual(e1.validator, "minimum")
        self.assertEqual(e1.validator_value, schema["anyOf"][0]["minimum"])
        self.assertEqual(e1.instance, instance)
        self.assertEqual(e1.schema, schema["anyOf"][0])
        self.assertIs(e1.parent, e)

        self.assertEqual(e1.path, deque([]))
        self.assertEqual(e1.absolute_path, deque([]))
        self.assertEqual(e1.relative_path, deque([]))

        self.assertEqual(e1.schema_path, deque([0, "minimum"]))
        self.assertEqual(e1.relative_schema_path, deque([0, "minimum"]))
        self.assertEqual(
            e1.absolute_schema_path, deque(["anyOf", 0, "minimum"]),
        )

        self.assertFalse(e1.context)

        self.assertEqual(e2.validator, "type")
        self.assertEqual(e2.validator_value, schema["anyOf"][1]["type"])
        self.assertEqual(e2.instance, instance)
        self.assertEqual(e2.schema, schema["anyOf"][1])
        self.assertIs(e2.parent, e)

        self.assertEqual(e2.path, deque([]))
        self.assertEqual(e2.relative_path, deque([]))
        self.assertEqual(e2.absolute_path, deque([]))

        self.assertEqual(e2.schema_path, deque([1, "type"]))
        self.assertEqual(e2.relative_schema_path, deque([1, "type"]))
        self.assertEqual(e2.absolute_schema_path, deque(["anyOf", 1, "type"]))

        self.assertEqual(len(e2.context), 0)

    def test_type(self):
        instance = {"foo": 1}
        schema = {
            "type": [
                {"type": "integer"},
                {
                    "type": "object",
                    "properties": {"foo": {"enum": [2]}},
                },
            ],
        }

        validator = Draft3Validator(schema)
        errors = list(validator.iter_errors(instance))
        self.assertEqual(len(errors), 1)
        e = errors[0]

        self.assertEqual(e.validator, "type")
        self.assertEqual(e.validator_value, schema["type"])
        self.assertEqual(e.instance, instance)
        self.assertEqual(e.schema, schema)
        self.assertIsNone(e.parent)

        self.assertEqual(e.path, deque([]))
        self.assertEqual(e.relative_path, deque([]))
        self.assertEqual(e.absolute_path, deque([]))

        self.assertEqual(e.schema_path, deque(["type"]))
        self.assertEqual(e.relative_schema_path, deque(["type"]))
        self.assertEqual(e.absolute_schema_path, deque(["type"]))

        self.assertEqual(len(e.context), 2)

        e1, e2 = sorted_errors(e.context)

        self.assertEqual(e1.validator, "type")
        self.assertEqual(e1.validator_value, schema["type"][0]["type"])
        self.assertEqual(e1.instance, instance)
        self.assertEqual(e1.schema, schema["type"][0])
        self.assertIs(e1.parent, e)

        self.assertEqual(e1.path, deque([]))
        self.assertEqual(e1.relative_path, deque([]))
        self.assertEqual(e1.absolute_path, deque([]))

        self.assertEqual(e1.schema_path, deque([0, "type"]))
        self.assertEqual(e1.relative_schema_path, deque([0, "type"]))
        self.assertEqual(e1.absolute_schema_path, deque(["type", 0, "type"]))

        self.assertFalse(e1.context)

        self.assertEqual(e2.validator, "enum")
        self.assertEqual(e2.validator_value, [2])
        self.assertEqual(e2.instance, 1)
        self.assertEqual(e2.schema, {u"enum": [2]})
        self.assertIs(e2.parent, e)

        self.assertEqual(e2.path, deque(["foo"]))
        self.assertEqual(e2.relative_path, deque(["foo"]))
        self.assertEqual(e2.absolute_path, deque(["foo"]))

        self.assertEqual(
            e2.schema_path, deque([1, "properties", "foo", "enum"]),
        )
        self.assertEqual(
            e2.relative_schema_path, deque([1, "properties", "foo", "enum"]),
        )
        self.assertEqual(
            e2.absolute_schema_path,
            deque(["type", 1, "properties", "foo", "enum"]),
        )

        self.assertFalse(e2.context)

    def test_single_nesting(self):
        instance = {"foo": 2, "bar": [1], "baz": 15, "quux": "spam"}
        schema = {
            "properties": {
                "foo": {"type": "string"},
                "bar": {"minItems": 2},
                "baz": {"maximum": 10, "enum": [2, 4, 6, 8]},
            },
        }

        validator = Draft3Validator(schema)
        errors = validator.iter_errors(instance)
        e1, e2, e3, e4 = sorted_errors(errors)

        self.assertEqual(e1.path, deque(["bar"]))
        self.assertEqual(e2.path, deque(["baz"]))
        self.assertEqual(e3.path, deque(["baz"]))
        self.assertEqual(e4.path, deque(["foo"]))

        self.assertEqual(e1.relative_path, deque(["bar"]))
        self.assertEqual(e2.relative_path, deque(["baz"]))
        self.assertEqual(e3.relative_path, deque(["baz"]))
        self.assertEqual(e4.relative_path, deque(["foo"]))

        self.assertEqual(e1.absolute_path, deque(["bar"]))
        self.assertEqual(e2.absolute_path, deque(["baz"]))
        self.assertEqual(e3.absolute_path, deque(["baz"]))
        self.assertEqual(e4.absolute_path, deque(["foo"]))

        self.assertEqual(e1.validator, "minItems")
        self.assertEqual(e2.validator, "enum")
        self.assertEqual(e3.validator, "maximum")
        self.assertEqual(e4.validator, "type")

    def test_multiple_nesting(self):
        instance = [1, {"foo": 2, "bar": {"baz": [1]}}, "quux"]
        schema = {
            "type": "string",
            "items": {
                "type": ["string", "object"],
                "properties": {
                    "foo": {"enum": [1, 3]},
                    "bar": {
                        "type": "array",
                        "properties": {
                            "bar": {"required": True},
                            "baz": {"minItems": 2},
                        },
                    },
                },
            },
        }

        validator = Draft3Validator(schema)
        errors = validator.iter_errors(instance)
        e1, e2, e3, e4, e5, e6 = sorted_errors(errors)

        self.assertEqual(e1.path, deque([]))
        self.assertEqual(e2.path, deque([0]))
        self.assertEqual(e3.path, deque([1, "bar"]))
        self.assertEqual(e4.path, deque([1, "bar", "bar"]))
        self.assertEqual(e5.path, deque([1, "bar", "baz"]))
        self.assertEqual(e6.path, deque([1, "foo"]))

        self.assertEqual(e1.schema_path, deque(["type"]))
        self.assertEqual(e2.schema_path, deque(["items", "type"]))
        self.assertEqual(
            list(e3.schema_path), ["items", "properties", "bar", "type"],
        )
        self.assertEqual(
            list(e4.schema_path),
            ["items", "properties", "bar", "properties", "bar", "required"],
        )
        self.assertEqual(
            list(e5.schema_path),
            ["items", "properties", "bar", "properties", "baz", "minItems"]
        )
        self.assertEqual(
            list(e6.schema_path), ["items", "properties", "foo", "enum"],
        )

        self.assertEqual(e1.validator, "type")
        self.assertEqual(e2.validator, "type")
        self.assertEqual(e3.validator, "type")
        self.assertEqual(e4.validator, "required")
        self.assertEqual(e5.validator, "minItems")
        self.assertEqual(e6.validator, "enum")

    def test_recursive(self):
        schema = {
            "definitions": {
                "node": {
                    "anyOf": [{
                        "type": "object",
                        "required": ["name", "children"],
                        "properties": {
                            "name": {
                                "type": "string",
                            },
                            "children": {
                                "type": "object",
                                "patternProperties": {
                                    "^.*$": {
                                        "$ref": "#/definitions/node",
                                    },
                                },
                            },
                        },
                    }],
                },
            },
            "type": "object",
            "required": ["root"],
            "properties": {"root": {"$ref": "#/definitions/node"}},
        }

        instance = {
            "root": {
                "name": "root",
                "children": {
                    "a": {
                        "name": "a",
                        "children": {
                            "ab": {
                                "name": "ab",
                                # missing "children"
                            },
                        },
                    },
                },
            },
        }
        validator = Draft4Validator(schema)

        e, = validator.iter_errors(instance)
        self.assertEqual(e.absolute_path, deque(["root"]))
        self.assertEqual(
            e.absolute_schema_path, deque(["properties", "root", "anyOf"]),
        )

        e1, = e.context
        self.assertEqual(e1.absolute_path, deque(["root", "children", "a"]))
        self.assertEqual(
            e1.absolute_schema_path, deque(
                [
                    "properties",
                    "root",
                    "anyOf",
                    0,
                    "properties",
                    "children",
                    "patternProperties",
                    "^.*$",
                    "anyOf",
                ],
            ),
        )

        e2, = e1.context
        self.assertEqual(
            e2.absolute_path, deque(
                ["root", "children", "a", "children", "ab"],
            ),
        )
        self.assertEqual(
            e2.absolute_schema_path, deque(
                [
                    "properties",
                    "root",
                    "anyOf",
                    0,
                    "properties",
                    "children",
                    "patternProperties",
                    "^.*$",
                    "anyOf",
                    0,
                    "properties",
                    "children",
                    "patternProperties",
                    "^.*$",
                    "anyOf",
                ],
            ),
        )

    def test_additionalProperties(self):
        instance = {"bar": "bar", "foo": 2}
        schema = {"additionalProperties": {"type": "integer", "minimum": 5}}

        validator = Draft3Validator(schema)
        errors = validator.iter_errors(instance)
        e1, e2 = sorted_errors(errors)

        self.assertEqual(e1.path, deque(["bar"]))
        self.assertEqual(e2.path, deque(["foo"]))

        self.assertEqual(e1.validator, "type")
        self.assertEqual(e2.validator, "minimum")

    def test_patternProperties(self):
        instance = {"bar": 1, "foo": 2}
        schema = {
            "patternProperties": {
                "bar": {"type": "string"},
                "foo": {"minimum": 5},
            },
        }

        validator = Draft3Validator(schema)
        errors = validator.iter_errors(instance)
        e1, e2 = sorted_errors(errors)

        self.assertEqual(e1.path, deque(["bar"]))
        self.assertEqual(e2.path, deque(["foo"]))

        self.assertEqual(e1.validator, "type")
        self.assertEqual(e2.validator, "minimum")

    def test_additionalItems(self):
        instance = ["foo", 1]
        schema = {
            "items": [],
            "additionalItems": {"type": "integer", "minimum": 5},
        }

        validator = Draft3Validator(schema)
        errors = validator.iter_errors(instance)
        e1, e2 = sorted_errors(errors)

        self.assertEqual(e1.path, deque([0]))
        self.assertEqual(e2.path, deque([1]))

        self.assertEqual(e1.validator, "type")
        self.assertEqual(e2.validator, "minimum")

    def test_additionalItems_with_items(self):
        instance = ["foo", "bar", 1]
        schema = {
            "items": [{}],
            "additionalItems": {"type": "integer", "minimum": 5},
        }

        validator = Draft3Validator(schema)
        errors = validator.iter_errors(instance)
        e1, e2 = sorted_errors(errors)

        self.assertEqual(e1.path, deque([1]))
        self.assertEqual(e2.path, deque([2]))

        self.assertEqual(e1.validator, "type")
        self.assertEqual(e2.validator, "minimum")


class ValidatorTestMixin(object):
    def setUp(self):
        self.instance = mock.Mock()
        self.schema = {}
        self.resolver = mock.Mock()
        self.validator = self.validator_class(self.schema)

    def test_valid_instances_are_valid(self):
        errors = iter([])

        with mock.patch.object(
            self.validator, "iter_errors", return_value=errors,
        ):
            self.assertTrue(
                self.validator.is_valid(self.instance, self.schema)
            )

    def test_invalid_instances_are_not_valid(self):
        errors = iter([mock.Mock()])

        with mock.patch.object(
            self.validator, "iter_errors", return_value=errors,
        ):
            self.assertFalse(
                self.validator.is_valid(self.instance, self.schema)
            )

    def test_non_existent_properties_are_ignored(self):
        instance, my_property, my_value = mock.Mock(), mock.Mock(), mock.Mock()
        validate(instance=instance, schema={my_property: my_value})

    def test_it_creates_a_ref_resolver_if_not_provided(self):
        self.assertIsInstance(self.validator.resolver, RefResolver)

    def test_it_delegates_to_a_ref_resolver(self):
        resolver = RefResolver("", {})
        schema = {"$ref": mock.Mock()}

        with mock.patch.object(resolver, "resolve") as resolve:
            resolve.return_value = "url", {"type": "integer"}
            with self.assertRaises(ValidationError):
                self.validator_class(schema, resolver=resolver).validate(None)

        resolve.assert_called_once_with(schema["$ref"])

    def test_it_delegates_to_a_legacy_ref_resolver(self):
        """
        Legacy RefResolvers support only the context manager form of
        resolution.

        """

        class LegacyRefResolver(object):
            @contextmanager
            def resolving(this, ref):
                self.assertEqual(ref, "the ref")
                yield {"type": "integer"}

        resolver = LegacyRefResolver()
        schema = {"$ref": "the ref"}

        with self.assertRaises(ValidationError):
            self.validator_class(schema, resolver=resolver).validate(None)

    def test_is_type_is_true_for_valid_type(self):
        self.assertTrue(self.validator.is_type("foo", "string"))

    def test_is_type_is_false_for_invalid_type(self):
        self.assertFalse(self.validator.is_type("foo", "array"))

    def test_is_type_evades_bool_inheriting_from_int(self):
        self.assertFalse(self.validator.is_type(True, "integer"))
        self.assertFalse(self.validator.is_type(True, "number"))

    def test_is_type_raises_exception_for_unknown_type(self):
        with self.assertRaises(UnknownType):
            self.validator.is_type("foo", object())


class TestDraft3Validator(ValidatorTestMixin, unittest.TestCase):
    validator_class = Draft3Validator

    def test_is_type_is_true_for_any_type(self):
        self.assertTrue(self.validator.is_valid(mock.Mock(), {"type": "any"}))

    def test_is_type_does_not_evade_bool_if_it_is_being_tested(self):
        self.assertTrue(self.validator.is_type(True, "boolean"))
        self.assertTrue(self.validator.is_valid(True, {"type": "any"}))

    def test_non_string_custom_types(self):
        schema = {'type': [None]}
        cls = self.validator_class(schema, types={None: type(None)})
        cls.validate(None, schema)


class TestDraft4Validator(ValidatorTestMixin, unittest.TestCase):
    validator_class = Draft4Validator


class TestBuiltinFormats(unittest.TestCase):
    """
    The built-in (specification-defined) formats do not raise type errors.

    If an instance or value is not a string, it should be ignored.

    """


for format in FormatChecker.checkers:
    def test(self, format=format):
        v = Draft4Validator({"format": format}, format_checker=FormatChecker())
        v.validate(123)

    name = "test_{0}_ignores_non_strings".format(format)
    test.__name__ = name
    setattr(TestBuiltinFormats, name, test)
    del test  # Ugh py.test. Stop discovering top level tests.


class TestValidatorFor(unittest.TestCase):
    def test_draft_3(self):
        schema = {"$schema": "http://json-schema.org/draft-03/schema"}
        self.assertIs(validator_for(schema), Draft3Validator)

        schema = {"$schema": "http://json-schema.org/draft-03/schema#"}
        self.assertIs(validator_for(schema), Draft3Validator)

    def test_draft_4(self):
        schema = {"$schema": "http://json-schema.org/draft-04/schema"}
        self.assertIs(validator_for(schema), Draft4Validator)

        schema = {"$schema": "http://json-schema.org/draft-04/schema#"}
        self.assertIs(validator_for(schema), Draft4Validator)

    def test_custom_validator(self):
        Validator = create(meta_schema={"id": "meta schema id"}, version="12")
        schema = {"$schema": "meta schema id"}
        self.assertIs(validator_for(schema), Validator)

    def test_validator_for_jsonschema_default(self):
        self.assertIs(validator_for({}), Draft4Validator)

    def test_validator_for_custom_default(self):
        self.assertIs(validator_for({}, default=None), None)


class TestValidate(unittest.TestCase):
    def test_draft3_validator_is_chosen(self):
        schema = {"$schema": "http://json-schema.org/draft-03/schema#"}
        with mock.patch.object(Draft3Validator, "check_schema") as chk_schema:
            validate({}, schema)
            chk_schema.assert_called_once_with(schema)
        # Make sure it works without the empty fragment
        schema = {"$schema": "http://json-schema.org/draft-03/schema"}
        with mock.patch.object(Draft3Validator, "check_schema") as chk_schema:
            validate({}, schema)
            chk_schema.assert_called_once_with(schema)

    def test_draft4_validator_is_chosen(self):
        schema = {"$schema": "http://json-schema.org/draft-04/schema#"}
        with mock.patch.object(Draft4Validator, "check_schema") as chk_schema:
            validate({}, schema)
            chk_schema.assert_called_once_with(schema)

    def test_draft4_validator_is_the_default(self):
        with mock.patch.object(Draft4Validator, "check_schema") as chk_schema:
            validate({}, {})
            chk_schema.assert_called_once_with({})


class TestRefResolver(unittest.TestCase):

    base_uri = ""
    stored_uri = "foo://stored"
    stored_schema = {"stored": "schema"}

    def setUp(self):
        self.referrer = {}
        self.store = {self.stored_uri: self.stored_schema}
        self.resolver = RefResolver(self.base_uri, self.referrer, self.store)

    def test_it_does_not_retrieve_schema_urls_from_the_network(self):
        ref = Draft3Validator.META_SCHEMA["id"]
        with mock.patch.object(self.resolver, "resolve_remote") as remote:
            with self.resolver.resolving(ref) as resolved:
                self.assertEqual(resolved, Draft3Validator.META_SCHEMA)
        self.assertFalse(remote.called)

    def test_it_resolves_local_refs(self):
        ref = "#/properties/foo"
        self.referrer["properties"] = {"foo": object()}
        with self.resolver.resolving(ref) as resolved:
            self.assertEqual(resolved, self.referrer["properties"]["foo"])

    def test_it_resolves_local_refs_with_id(self):
        schema = {"id": "http://bar/schema#", "a": {"foo": "bar"}}
        resolver = RefResolver.from_schema(schema)
        with resolver.resolving("#/a") as resolved:
            self.assertEqual(resolved, schema["a"])
        with resolver.resolving("http://bar/schema#/a") as resolved:
            self.assertEqual(resolved, schema["a"])

    def test_it_retrieves_stored_refs(self):
        with self.resolver.resolving(self.stored_uri) as resolved:
            self.assertIs(resolved, self.stored_schema)

        self.resolver.store["cached_ref"] = {"foo": 12}
        with self.resolver.resolving("cached_ref#/foo") as resolved:
            self.assertEqual(resolved, 12)

    def test_it_retrieves_unstored_refs_via_requests(self):
        ref = "http://bar#baz"
        schema = {"baz": 12}

        with mock.patch("jsonschema.validators.requests") as requests:
            requests.get.return_value.json.return_value = schema
            with self.resolver.resolving(ref) as resolved:
                self.assertEqual(resolved, 12)
        requests.get.assert_called_once_with("http://bar")

    def test_it_retrieves_unstored_refs_via_urlopen(self):
        ref = "http://bar#baz"
        schema = {"baz": 12}

        with mock.patch("jsonschema.validators.requests", None):
            with mock.patch("jsonschema.validators.urlopen") as urlopen:
                urlopen.return_value.read.return_value = (
                    json.dumps(schema).encode("utf8"))
                with self.resolver.resolving(ref) as resolved:
                    self.assertEqual(resolved, 12)
        urlopen.assert_called_once_with("http://bar")

    def test_it_can_construct_a_base_uri_from_a_schema(self):
        schema = {"id": "foo"}
        resolver = RefResolver.from_schema(schema)
        self.assertEqual(resolver.base_uri, "foo")
        self.assertEqual(resolver.resolution_scope, "foo")
        with resolver.resolving("") as resolved:
            self.assertEqual(resolved, schema)
        with resolver.resolving("#") as resolved:
            self.assertEqual(resolved, schema)
        with resolver.resolving("foo") as resolved:
            self.assertEqual(resolved, schema)
        with resolver.resolving("foo#") as resolved:
            self.assertEqual(resolved, schema)

    def test_it_can_construct_a_base_uri_from_a_schema_without_id(self):
        schema = {}
        resolver = RefResolver.from_schema(schema)
        self.assertEqual(resolver.base_uri, "")
        self.assertEqual(resolver.resolution_scope, "")
        with resolver.resolving("") as resolved:
            self.assertEqual(resolved, schema)
        with resolver.resolving("#") as resolved:
            self.assertEqual(resolved, schema)

    def test_custom_uri_scheme_handlers(self):
        schema = {"foo": "bar"}
        ref = "foo://bar"
        foo_handler = mock.Mock(return_value=schema)
        resolver = RefResolver("", {}, handlers={"foo": foo_handler})
        with resolver.resolving(ref) as resolved:
            self.assertEqual(resolved, schema)
        foo_handler.assert_called_once_with(ref)

    def test_cache_remote_on(self):
        ref = "foo://bar"
        foo_handler = mock.Mock()
        resolver = RefResolver(
            "", {}, cache_remote=True, handlers={"foo": foo_handler},
        )
        with resolver.resolving(ref):
            pass
        with resolver.resolving(ref):
            pass
        foo_handler.assert_called_once_with(ref)

    def test_cache_remote_off(self):
        ref = "foo://bar"
        foo_handler = mock.Mock()
        resolver = RefResolver(
            "", {}, cache_remote=False, handlers={"foo": foo_handler},
        )
        with resolver.resolving(ref):
            pass
        self.assertEqual(foo_handler.call_count, 1)

    def test_if_you_give_it_junk_you_get_a_resolution_error(self):
        ref = "foo://bar"
        foo_handler = mock.Mock(side_effect=ValueError("Oh no! What's this?"))
        resolver = RefResolver("", {}, handlers={"foo": foo_handler})
        with self.assertRaises(RefResolutionError) as err:
            with resolver.resolving(ref):
                pass
        self.assertEqual(str(err.exception), "Oh no! What's this?")

    def test_helpful_error_message_on_failed_pop_scope(self):
        resolver = RefResolver("", {})
        resolver.pop_scope()
        with self.assertRaises(RefResolutionError) as exc:
            resolver.pop_scope()
        self.assertIn("Failed to pop the scope", str(exc.exception))


class UniqueTupleItemsMixin(object):
    """
    A tuple instance properly formats validation errors for uniqueItems.

    See https://github.com/Julian/jsonschema/pull/224

    """

    def test_it_properly_formats_an_error_message(self):
        validator = self.validator_class(
            schema={"uniqueItems": True},
            types={"array": (tuple,)},
        )
        with self.assertRaises(ValidationError) as e:
            validator.validate((1, 1))
        self.assertIn("(1, 1) has non-unique elements", str(e.exception))


class TestDraft4UniqueTupleItems(UniqueTupleItemsMixin, unittest.TestCase):
    validator_class = Draft4Validator


class TestDraft3UniqueTupleItems(UniqueTupleItemsMixin, unittest.TestCase):
    validator_class = Draft3Validator


def sorted_errors(errors):
    def key(error):
        return (
            [str(e) for e in error.path],
            [str(e) for e in error.schema_path],
        )
    return sorted(errors, key=key)
