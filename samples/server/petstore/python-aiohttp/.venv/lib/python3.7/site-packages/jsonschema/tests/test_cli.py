from jsonschema import Draft4Validator, ValidationError, cli
from jsonschema.compat import StringIO
from jsonschema.exceptions import SchemaError
from jsonschema.tests.compat import mock, unittest


def fake_validator(*errors):
    errors = list(reversed(errors))

    class FakeValidator(object):
        def __init__(self, *args, **kwargs):
            pass

        def iter_errors(self, instance):
            if errors:
                return errors.pop()
            return []

        def check_schema(self, schema):
            pass

    return FakeValidator


class TestParser(unittest.TestCase):
    FakeValidator = fake_validator()

    def setUp(self):
        mock_open = mock.mock_open()
        patch_open = mock.patch.object(cli, "open", mock_open, create=True)
        patch_open.start()
        self.addCleanup(patch_open.stop)

        mock_json_load = mock.Mock()
        mock_json_load.return_value = {}
        patch_json_load = mock.patch("json.load")
        patch_json_load.start()
        self.addCleanup(patch_json_load.stop)

    def test_find_validator_by_fully_qualified_object_name(self):
        arguments = cli.parse_args(
            [
                "--validator",
                "jsonschema.tests.test_cli.TestParser.FakeValidator",
                "--instance", "foo.json",
                "schema.json",
            ]
        )
        self.assertIs(arguments["validator"], self.FakeValidator)

    def test_find_validator_in_jsonschema(self):
        arguments = cli.parse_args(
            [
                "--validator", "Draft4Validator",
                "--instance", "foo.json",
                "schema.json",
            ]
        )
        self.assertIs(arguments["validator"], Draft4Validator)


class TestCLI(unittest.TestCase):
    def test_draft3_schema_draft4_validator(self):
        stdout, stderr = StringIO(), StringIO()
        with self.assertRaises(SchemaError):
            cli.run(
                {
                    "validator": Draft4Validator,
                    "schema": {
                        "anyOf": [
                            {"minimum": 20},
                            {"type": "string"},
                            {"required": True},
                        ],
                    },
                    "instances": [1],
                    "error_format": "{error.message}",
                },
                stdout=stdout,
                stderr=stderr,
            )

    def test_successful_validation(self):
        stdout, stderr = StringIO(), StringIO()
        exit_code = cli.run(
            {
                "validator": fake_validator(),
                "schema": {},
                "instances": [1],
                "error_format": "{error.message}",
            },
            stdout=stdout,
            stderr=stderr,
        )
        self.assertFalse(stdout.getvalue())
        self.assertFalse(stderr.getvalue())
        self.assertEqual(exit_code, 0)

    def test_unsuccessful_validation(self):
        error = ValidationError("I am an error!", instance=1)
        stdout, stderr = StringIO(), StringIO()
        exit_code = cli.run(
            {
                "validator": fake_validator([error]),
                "schema": {},
                "instances": [1],
                "error_format": "{error.instance} - {error.message}",
            },
            stdout=stdout,
            stderr=stderr,
        )
        self.assertFalse(stdout.getvalue())
        self.assertEqual(stderr.getvalue(), "1 - I am an error!")
        self.assertEqual(exit_code, 1)

    def test_unsuccessful_validation_multiple_instances(self):
        first_errors = [
            ValidationError("9", instance=1),
            ValidationError("8", instance=1),
        ]
        second_errors = [ValidationError("7", instance=2)]
        stdout, stderr = StringIO(), StringIO()
        exit_code = cli.run(
            {
                "validator": fake_validator(first_errors, second_errors),
                "schema": {},
                "instances": [1, 2],
                "error_format": "{error.instance} - {error.message}\t",
            },
            stdout=stdout,
            stderr=stderr,
        )
        self.assertFalse(stdout.getvalue())
        self.assertEqual(stderr.getvalue(), "1 - 9\t1 - 8\t2 - 7\t")
        self.assertEqual(exit_code, 1)
