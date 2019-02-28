# coding: utf-8

# flake8: noqa

"""
Run the tests.
$ pip install nose (optional)
$ cd OpenAPIPetstore-python
$ nosetests -v
"""
from collections import namedtuple
import json
import os
import six
import time
import unittest

from dateutil.tz import tzoffset

import petstore_api
from petstore_api import (
    AdditionalPropertiesAnyType,
    AdditionalPropertiesClass,
    Category,
    Dog,
    EnumTest,
    OuterEnum,
    Pet,
    Tag
)
from petstore_api.exceptions import (
    ApiTypeError,
    ApiKeyError,
    ApiValueError,
)
from petstore_api.model_utils import (
    date,
    datetime,
    file_type,
    int,
    model_to_dict,
    none_type,
    str
)

MockResponse = namedtuple('MockResponse', 'data')

ANYTYPE_PAIRS = [
    ("7", "7"),
    ("what", "what"),
    (5, 5),
    (2147483647, 2147483647),  # int32 max
    (-2147483648, -2147483648),  # int32 min
    (9223372036854775807, 9223372036854775807),  # int 64 max
    (-9223372036854775808, -9223372036854775808),  # int 64 min
    (-3.1415, -3.1415),
    (True, True),
    (False, False),
    ({}, {}),
    ({'hi': 'there'}, {'hi': 'there'}),
    ({'hi': None}, {'hi': None}),
    ([], []),
    ([None], [None]),
    ([0, 1], [0, 1]),
    (['a', 2.7], ['a', 2.7]),
    ("1997-07-16", date(1997, 7, 16)),
    (
        "1997-07-16T19:20:30.45+01:00",
        datetime(1997, 7, 16, 19, 20, 30, 450000,
                 tzinfo=tzoffset(None, 3600))
    ),
]

class DeserializationTests(unittest.TestCase):

    def setUp(self):
        self.api_client = petstore_api.ApiClient()
        self.deserialize = self.api_client.deserialize

    def test_enum_test(self):
        """ deserialize dict(str, Enum_Test) """
        data = {
            'enum_test': {
                "enum_string": "UPPER",
                "enum_string_required": "lower",
                "enum_integer": 1,
                "enum_number": 1.1,
                "outerEnum": "placed"
            }
        }
        response = MockResponse(data=json.dumps(data))
        # ignore the first bracket and read tuples as 'of'
        # [{str: (EnumTest,)}] -> {str: (EnumTest,)} dict of EnumTest instances
        response_types_mixed = [{str: (EnumTest,)}]

        deserialized = self.deserialize(response, response_types_mixed)
        self.assertTrue(isinstance(deserialized, dict))
        self.assertTrue(isinstance(deserialized['enum_test'], EnumTest))
        self.assertEqual(
            deserialized['enum_test'],
            EnumTest(
                enum_string="UPPER",
                enum_string_required="lower",
                enum_integer=1,
                enum_number=1.1,
                outer_enum=OuterEnum.PLACED
            )
        )

    def test_deserialize_dict_str_pet(self):
        """ deserialize dict(str, Pet) """
        data = {
            'pet': {
                "id": 0,
                "category": {
                    "id": 0,
                    "name": "string"
                },
                "name": "doggie",
                "photoUrls": [
                    "string"
                ],
                "tags": [
                    {
                        "id": 0,
                        "name": "string"
                    }
                ],
                "status": "available"
            }
        }
        response = MockResponse(data=json.dumps(data))
        response_types_mixed = [{str: (Pet,)}]

        deserialized = self.deserialize(response, response_types_mixed)
        self.assertTrue(isinstance(deserialized, dict))
        self.assertTrue(isinstance(deserialized['pet'], Pet))

    def test_deserialize_dict_str_dog(self):
        """ deserialize dict(str, Dog), use discriminator"""
        data = {
            'dog': {
                "className": "Dog",
                "color": "white",
                "breed": "Jack Russel Terrier"
            }
        }
        response = MockResponse(data=json.dumps(data))
        response_types_mixed = [{str: (Dog,)}]

        deserialized = self.deserialize(response, response_types_mixed)
        self.assertTrue(isinstance(deserialized, dict))
        self.assertTrue(isinstance(deserialized['dog'], Dog))

    def test_deserialize_dict_str_int(self):
        """ deserialize dict(str, int) """
        data = {
            'integer': 1
        }
        response = MockResponse(data=json.dumps(data))
        response_types_mixed = [{str: (int,)}]

        deserialized = self.deserialize(response, response_types_mixed)
        self.assertTrue(isinstance(deserialized, dict))
        self.assertTrue(isinstance(deserialized['integer'], int))
        self.assertEqual(deserialized, data)

    def test_deserialize_str(self):
        """ deserialize str """
        data = "test str"
        response = MockResponse(data=json.dumps(data))
        response_types_mixed = [str]

        deserialized = self.deserialize(response, response_types_mixed)
        self.assertTrue(isinstance(deserialized, str))
        self.assertEqual(deserialized, data)

    def test_deserialize_str_to_int(self):
        """ deserialize str """

        input_output_pairs = (
            ("2", 2),
            ("2147483647", 2147483647),  # int32 max
            ("-2147483648", -2147483648),  # int32 min
            ("9223372036854775807", 9223372036854775807),  # int 64 max
            ("-9223372036854775808", -9223372036854775808),  # int 64 min
        )
        response_types_mixed = [int]

        for input, output in input_output_pairs:
            response = MockResponse(data=json.dumps(input))
            deserialized = self.deserialize(response, response_types_mixed)
            self.assertTrue(isinstance(deserialized, int))
            self.assertEqual(deserialized, output)


    def test_deserialize_date(self):
        """ deserialize date """
        data = "1997-07-16"
        response = MockResponse(data=json.dumps(data))
        response_types_mixed = [date]

        deserialized = self.deserialize(response, response_types_mixed)
        self.assertTrue(isinstance(deserialized, date))
        correct_date = date(1997, 7, 16)
        self.assertEqual(deserialized, correct_date)

    def test_deserialize_datetime(self):
        """ deserialize datetime """
        data = "1997-07-16T19:20:30.45+01:00"
        response_types_mixed = [datetime]
        response = MockResponse(data=json.dumps(data))

        deserialized = self.deserialize(response, response_types_mixed)
        self.assertTrue(isinstance(deserialized, datetime))
        correct_datetime = datetime(1997, 7, 16, 19, 20, 30, 450000,
            tzinfo=tzoffset(None, 3600))
        self.assertEqual(deserialized, correct_datetime)

    def test_deserialize_datetime_to_date(self):
        """ deserialize datetime to date """
        data = "1997-07-16T19:20:30.45+01:00"
        response_types_mixed = [date]
        response = MockResponse(data=json.dumps(data))

        deserialized = self.deserialize(response, response_types_mixed)
        self.assertTrue(isinstance(deserialized, date))
        correct_date = date(1997, 7, 16)
        self.assertEqual(deserialized, correct_date)

    def test_deserialize_pet(self):
        """ deserialize pet """
        data = {
            "id": 0,
            "category": {
                "id": 0,
                "name": "string"
            },
            "name": "doggie",
            "photoUrls": [
                "string"
            ],
            "tags": [
                {
                    "id": 0,
                    "name": "string"
                }
            ],
            "status": "available"
        }
        response = MockResponse(data=json.dumps(data))
        response_types_mixed = [Pet]

        deserialized = self.deserialize(response, response_types_mixed)
        self.assertTrue(isinstance(deserialized, Pet))
        self.assertEqual(deserialized.id, 0)
        self.assertEqual(deserialized.name, "doggie")
        self.assertTrue(isinstance(deserialized.category, Category))
        self.assertEqual(deserialized.category.name, "string")
        self.assertEqual(deserialized.category.id, 0)
        self.assertTrue(isinstance(deserialized.tags, list))
        self.assertTrue(isinstance(deserialized.tags[0], Tag))
        self.assertEqual(deserialized.tags[0].name, "string")
        self.assertEqual(deserialized.tags[0].id, 0)
        self.assertEqual(deserialized.status, "available")
        self.assertEqual(deserialized.status, "available")
        self.assertEqual(data, model_to_dict(deserialized, serialize=True))

    def test_deserialize_list_of_pet(self):
        """ deserialize list[Pet] """
        data = [
            {
                "id": 0,
                "category": {
                    "id": 0,
                    "name": "string"
                },
                "name": "doggie0",
                "photoUrls": [
                    "string"
                ],
                "tags": [
                    {
                        "id": 0,
                        "name": "string"
                    }
                ],
                "status": "available"
            },
            {
                "id": 1,
                "category": {
                    "id": 0,
                    "name": "string"
                },
                "name": "doggie1",
                "photoUrls": [
                    "string"
                ],
                "tags": [
                    {
                        "id": 0,
                        "name": "string"
                    }
                ],
                "status": "available"
            }]
        response = MockResponse(data=json.dumps(data))
        response_types_mixed = [[(Pet,)]]

        deserialized = self.deserialize(response, response_types_mixed)
        self.assertTrue(isinstance(deserialized, list))
        self.assertTrue(isinstance(deserialized[0], Pet))
        self.assertEqual(deserialized[0].id, 0)
        self.assertEqual(deserialized[1].id, 1)
        self.assertEqual(deserialized[0].name, "doggie0")
        self.assertEqual(deserialized[1].name, "doggie1")

    def test_deserialize_failure_keyerror(self):
        """ deserialize list[Pet] """
        data = [
            {
                "id": 0,
                "category": {
                    "id": 0,
                    "name": "string"
                },
                "name": "doggie0",
                "photoUrls": [
                    "string"
                ],
                "tags": [
                    {
                        "id": 0,
                        "name": "string"
                    }
                ],
                "status": "available"
            },
            {
                "id": 1,
                "category": {
                    "id": 0,
                    "name": "string"
                },
                "namee": "doggie1",
                "photoUrls": [
                    "string"
                ],
                "tags": [
                    {
                        "id": 0,
                        "name": "string"
                    }
                ],
                "status": "available"
            }]
        response = MockResponse(data=json.dumps(data))
        response_types_mixed = [[(Pet,)]]

        error_msg = ()
        with self.assertRaises(ApiKeyError) as exc:
            deserialized = self.deserialize(response, response_types_mixed)
        self.assertEqual(str(exc.exception),
            "\"Pet has no key 'namee' at ['received_data'][1]['namee']\"")

    def test_deserialize_failure_typeerror(self):
        """ deserialize list[Pet] """
        data = [
            {
                "id": 0,
                "category": {
                    "id": 0,
                    "name": "string"
                },
                "name": "doggie0",
                "photoUrls": [
                    "string"
                ],
                "tags": [
                    {
                        "id": 0,
                        "name": "string"
                    }
                ],
                "status": "available"
            },
            {
                "id": None,
                "category": {
                    "id": 0,
                    "name": "string"
                },
                "name": "doggie1",
                "photoUrls": [
                    "string"
                ],
                "tags": [
                    {
                        "id": 0,
                        "name": "string"
                    }
                ],
                "status": "available"
            }]
        response = MockResponse(data=json.dumps(data))
        response_types_mixed = [[(Pet,)]]

        with self.assertRaises(ApiTypeError) as exc:
            deserialized = self.deserialize(response, response_types_mixed)
        exception_str = str(exc.exception)
        py2_error = (
            "Invalid type for variable 'id'. Required value type is "
            "one of [int, long, newint] and passed type was NoneType at "
            "['received_data'][1]['id']"
        )
        py3_error = (
            "Invalid type for variable 'id'. Required value type is int "
            "and passed type was NoneType at ['received_data'][1]['id']"
        )
        if six.PY2:
            self.assertEqual(exception_str, py2_error)
        else:
            self.assertEqual(exception_str, py3_error)

    def test_deserialize_failure_valueerror(self):
        """ deserialize list[Pet] """
        data = [
            {
                "id": 0,
                "category": {
                    "id": 0,
                    "name": "string"
                },
                "name": "doggie0",
                "photoUrls": [
                    "string"
                ],
                "tags": [
                    {
                        "id": 0,
                        "name": "string"
                    }
                ],
                "status": "available"
            },
            {
                "id": 'a1',
                "category": {
                    "id": 0,
                    "name": "string"
                },
                "name": "doggie1",
                "photoUrls": [
                    "string"
                ],
                "tags": [
                    {
                        "id": 0,
                        "name": "string"
                    }
                ],
                "status": "available"
            }]
        response = MockResponse(data=json.dumps(data))
        response_types_mixed = [[(Pet,)]]

        # Failed to parse 'a1' as int at ['received_data'][1]['id']
        error_msg = (r"Failed to parse [u]?\'a1\' as int at "
                     r"\[\'received_data\'\]\[1\]\[\'id\'\]")
        with self.assertRaisesRegexp(ApiValueError, error_msg) as exc:
            deserialized = self.deserialize(response, response_types_mixed)

    def test_deserialize_nested_dict(self):
        """ deserialize dict(str, dict(str, int)) """
        data = {
            "foo": {
                "bar": 1
            }
        }
        response = MockResponse(data=json.dumps(data))
        response_types_mixed = [{str: ({str: (int,)},)}]

        deserialized = self.deserialize(response, response_types_mixed)
        self.assertTrue(isinstance(deserialized, dict))
        self.assertTrue(isinstance(deserialized["foo"], dict))
        self.assertTrue(isinstance(deserialized["foo"]["bar"], int))
        self.assertEqual(deserialized, data)

    def test_deserialize_nested_list(self):
        """ deserialize list[list[str]] """
        data = [["foo"]]
        response = MockResponse(data=json.dumps(data))
        response_types_mixed = [[([(str,)],)]]

        deserialized = self.deserialize(response, response_types_mixed)
        self.assertTrue(isinstance(deserialized, list))
        self.assertTrue(isinstance(deserialized[0], list))
        self.assertTrue(isinstance(deserialized[0][0], str))
        self.assertEqual(deserialized, data)

    def test_deserialize_list_to_class_instance(self):
        """ deserialize list to model """
        photo_urls = ['http://www.example.com/image.jpg']
        # this is the only required property, which is a positional argument
        data = [photo_urls]
        response = MockResponse(data=json.dumps(data))
        response_types_mixed = [Pet]

        deserialized = self.deserialize(response, response_types_mixed)
        self.assertTrue(isinstance(deserialized, Pet))
        self.assertEqual(deserialized.photo_urls, photo_urls)

    def test_deserialize_str_to_int_failure(self):
        """ try to deserialize bad string into int """
        data = {'hi': 'a23'}
        response = MockResponse(data=json.dumps(data))
        response_types_mixed = [{str: (int,)}]

        # python2 casts the 'a23' as unicode and has a prefex u in its repr
        # Failed to parse 'a23' as int at ['received_data']['hi']
        error_msg = (r"Failed to parse [u]?\'a23\' as int at "
                     r"\[\'received_data\'\]\[\'hi\'\]")
        with self.assertRaisesRegexp(ApiValueError, error_msg) as exc:
            deserialized = self.deserialize(response, response_types_mixed)

    def test_deserialize_str_to_date_failure(self):
        """ try to deserialize bad string into int """
        string_inputs = [str('bogus'), u'bogus', 'bogus']
        response_types_mixed = [date]

        for string_input in string_inputs:
            response = MockResponse(data=json.dumps(string_input))

            # If you need your parameter to have a fallback string value,
            # please set its type as `type: {}` in your spec. That allows the
            # value to be any type. Failed to parse u'bogus' as date at
            # ['received_data']
            error_msg = (
                r"If you need your parameter to have a fallback string value, "
                r"please set its type as \`type: \{\}\` in your spec\. That "
                r"allows the value to be any type\. Failed to parse "
                r"[u]?\'bogus\' as date at \[\'received_data\'\]"
            )
            with self.assertRaisesRegexp(ApiValueError, error_msg) as exc:
                deserialized = self.deserialize(response, response_types_mixed)


    def test_deserialize_none_failure(self):
        """ deserialize None """
        data = None
        response = MockResponse(data=json.dumps(data))
        response_types_mixed = [datetime]

        error_msg = (
            "Invalid type for variable 'received_data'. Required value type is "
            "datetime and passed type was NoneType at ['received_data']"
        )
        with self.assertRaises(ApiTypeError) as exc:
            deserialized = self.deserialize(response, response_types_mixed)
        self.assertEqual(str(exc.exception), error_msg)

    def test_deserialize_nullable_success(self):
        """ deserialize datatetime/None """
        response_types_mixed = [date, none_type]

        data = None
        response = MockResponse(data=json.dumps(data))
        deserialized = self.deserialize(response, response_types_mixed)
        self.assertTrue(isinstance(deserialized, none_type))
        self.assertEqual(deserialized, None)

        data = "1997-07-16"
        response = MockResponse(data=json.dumps(data))
        deserialized = self.deserialize(response, response_types_mixed)
        self.assertTrue(isinstance(deserialized, date))
        self.assertEqual(deserialized, date(1997, 7, 16))

    def test_deserialize_anytype_primitive(self):
        response_types_mixed = [
            bool, date, datetime, dict, float, int, list, str
        ]
        for input, output in ANYTYPE_PAIRS:
            response = MockResponse(data=json.dumps(input))
            deserialized = self.deserialize(response, response_types_mixed)
            self.assertEqual(deserialized, output)

    def test_deserialize_anytype_primitive_failure(self):
        response_types_mixed = [
            bool, date, datetime, dict, float, int, list, str
        ]
        response = MockResponse(data=json.dumps(None))
        with self.assertRaises(ApiTypeError) as exc:
            deserialized = self.deserialize(response, response_types_mixed)
        exception_str = str(exc.exception)
        py2_error = (
            "Invalid type for variable 'received_data'. Required value type is"
            " one of [bool, date, datetime, dict, float, int, list, long, "
            "newint, newstr, str, unicode] and passed type was NoneType at "
            "['received_data']"
        )
        py3_error = (
            "Invalid type for variable 'received_data'. Required value type "
            "is one of [bool, date, datetime, dict, float, int, list, str] "
            "and passed type was NoneType at ['received_data']"
        )
        if six.PY2:
            self.assertEqual(exception_str, py2_error)
        else:
            self.assertEqual(exception_str, py3_error)


    def test_deserialize_anytype_list(self):
        response_types_mixed = [
            [(bool, date, datetime, dict, float, int, list, str)]
        ]
        for input, output in ANYTYPE_PAIRS:
            response = MockResponse(data=json.dumps([input]))
            deserialized = self.deserialize(response, response_types_mixed)
            self.assertEqual(deserialized, [output])

    def test_deserialize_anytype_dict(self):
        response_types_mixed = [
            {
                str: (bool, date, datetime, dict, float, int, list, str)
            }
        ]
        string_keys = [str('bogus'), u'bogus', 'bogus']
        for string_key in string_keys:
            for input, output in ANYTYPE_PAIRS:
                response = MockResponse(data=json.dumps({string_key: input}))
                deserialized = self.deserialize(response, response_types_mixed)
                self.assertEqual(deserialized, {string_key: output})

    def test_deserialize_anytype_model_prop(self):
        response_types_mixed = [AdditionalPropertiesClass]
        for input, output in ANYTYPE_PAIRS:
            response = MockResponse(data=json.dumps(dict(anytype_1=input)))
            deserialized = self.deserialize(response, response_types_mixed)
            self.assertTrue(isinstance(deserialized, AdditionalPropertiesClass))
            self.assertEqual(deserialized.anytype_1, output)

    def test_deserialize_anytype_additionalproperties_prop(self):
        response_types_mixed = [AdditionalPropertiesAnyType]
        for input, output in ANYTYPE_PAIRS:
            response = MockResponse(data=json.dumps(dict(some_var=input)))
            deserialized = self.deserialize(response, response_types_mixed)
            self.assertTrue(isinstance(deserialized, AdditionalPropertiesAnyType))
            self.assertEqual(deserialized['some_var'], output)
