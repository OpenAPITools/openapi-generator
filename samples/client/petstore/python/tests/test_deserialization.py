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
import time
import unittest

from dateutil.tz import tzoffset

import petstore_api
from petstore_api import (
    Category,
    Dog,
    EnumTest,
    OuterEnum,
    Pet,
    Tag
)
from petstore_api.utils import (
    ApiTypeError,
    ApiValueError,
    date,
    datetime,
    file_type,
    model_to_dict,
    none_type
)

MockResponse = namedtuple('MockResponse', 'data')


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

    def test_deserialize_list_of_pet_failure(self):
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
        # ignore the first bracket and read tuples as 'of'
        # [[([(str,)],)]] -> [([(str,)],)] list of list of str
        response_types_mixed = [[([(str,)],)]]

        deserialized = self.deserialize(response, response_types_mixed)
        self.assertTrue(isinstance(deserialized, list))
        self.assertTrue(isinstance(deserialized[0], list))
        self.assertTrue(isinstance(deserialized[0][0], str))
        self.assertEqual(deserialized, data)

    def test_deserialize_none_failure(self):
        """ deserialize None """
        data = None
        response = MockResponse(data=json.dumps(data))
        response_types_mixed = [datetime]

        with self.assertRaises(TypeError) as exc:
            deserialized = self.deserialize(response, response_types_mixed)
        error_msg = (
            "Invalid type for variable received_data. Required value type is "
            "(<class 'datetime.datetime'>,) and passed type was <class "
            "'NoneType'> at location=['received_data']"
        )
        self.assertEqual(str(exc.exception), error_msg)

    def test_deserialize_nullable_success(self):
        """ deserialize datatetime/None """
        data = None
        response = MockResponse(data=json.dumps(data))
        response_types_mixed = [datetime, none_type]

        deserialized = self.deserialize(response, response_types_mixed)
        self.assertEqual(deserialized, None)
