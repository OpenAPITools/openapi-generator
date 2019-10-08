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
import datetime

import petstore_api


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

        deserialized = self.deserialize(response, 'dict(str, EnumTest)')
        self.assertTrue(isinstance(deserialized, dict))
        self.assertTrue(
            isinstance(deserialized['enum_test'], petstore_api.EnumTest))
        outer_enum_value = (
            petstore_api.OuterEnum.allowed_values[('value',)]["PLACED"])
        outer_enum = petstore_api.OuterEnum(outer_enum_value)
        sample_instance = petstore_api.EnumTest(
            enum_string="UPPER",
            enum_string_required="lower",
            enum_integer=1,
            enum_number=1.1,
            outer_enum=outer_enum
        )
        self.assertEqual(deserialized['enum_test'], sample_instance)

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

        deserialized = self.deserialize(response, 'dict(str, Pet)')
        self.assertTrue(isinstance(deserialized, dict))
        self.assertTrue(isinstance(deserialized['pet'], petstore_api.Pet))

    def test_deserialize_dict_str_dog(self):
        """ deserialize dict(str, Dog), use discriminator"""
        data = {
            'dog': {
                "id": 0,
                "className": "Dog",
                "color": "white",
                "bread": "Jack Russel Terrier"
            }
        }
        response = MockResponse(data=json.dumps(data))

        deserialized = self.deserialize(response, 'dict(str, Animal)')
        self.assertTrue(isinstance(deserialized, dict))
        self.assertTrue(isinstance(deserialized['dog'], petstore_api.Dog))

    def test_deserialize_dict_str_int(self):
        """ deserialize dict(str, int) """
        data = {
            'integer': 1
        }
        response = MockResponse(data=json.dumps(data))

        deserialized = self.deserialize(response, 'dict(str, int)')
        self.assertTrue(isinstance(deserialized, dict))
        self.assertTrue(isinstance(deserialized['integer'], int))

    def test_deserialize_str(self):
        """ deserialize str """
        data = "test str"
        response = MockResponse(data=json.dumps(data))

        deserialized = self.deserialize(response, "str")
        self.assertTrue(isinstance(deserialized, str))

    def test_deserialize_date(self):
        """ deserialize date """
        data = "1997-07-16"
        response = MockResponse(data=json.dumps(data))

        deserialized = self.deserialize(response, "date")
        self.assertTrue(isinstance(deserialized, datetime.date))

    def test_deserialize_datetime(self):
        """ deserialize datetime """
        data = "1997-07-16T19:20:30.45+01:00"
        response = MockResponse(data=json.dumps(data))

        deserialized = self.deserialize(response, "datetime")
        self.assertTrue(isinstance(deserialized, datetime.datetime))

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

        deserialized = self.deserialize(response, "Pet")
        self.assertTrue(isinstance(deserialized, petstore_api.Pet))
        self.assertEqual(deserialized.id, 0)
        self.assertEqual(deserialized.name, "doggie")
        self.assertTrue(isinstance(deserialized.category, petstore_api.Category))
        self.assertEqual(deserialized.category.name, "string")
        self.assertTrue(isinstance(deserialized.tags, list))
        self.assertEqual(deserialized.tags[0].name, "string")

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

        deserialized = self.deserialize(response, "list[Pet]")
        self.assertTrue(isinstance(deserialized, list))
        self.assertTrue(isinstance(deserialized[0], petstore_api.Pet))
        self.assertEqual(deserialized[0].id, 0)
        self.assertEqual(deserialized[1].id, 1)
        self.assertEqual(deserialized[0].name, "doggie0")
        self.assertEqual(deserialized[1].name, "doggie1")

    def test_deserialize_nested_dict(self):
        """ deserialize dict(str, dict(str, int)) """
        data = {
            "foo": {
                "bar": 1
            }
        }
        response = MockResponse(data=json.dumps(data))

        deserialized = self.deserialize(response, "dict(str, dict(str, int))")
        self.assertTrue(isinstance(deserialized, dict))
        self.assertTrue(isinstance(deserialized["foo"], dict))
        self.assertTrue(isinstance(deserialized["foo"]["bar"], int))

    def test_deserialize_nested_list(self):
        """ deserialize list[list[str]] """
        data = [["foo"]]
        response = MockResponse(data=json.dumps(data))

        deserialized = self.deserialize(response, "list[list[str]]")
        self.assertTrue(isinstance(deserialized, list))
        self.assertTrue(isinstance(deserialized[0], list))
        self.assertTrue(isinstance(deserialized[0][0], str))

    def test_deserialize_none(self):
        """ deserialize None """
        response = MockResponse(data=json.dumps(None))

        deserialized = self.deserialize(response, "datetime")
        self.assertIsNone(deserialized)

    def test_deserialize_OuterEnum(self):
        """ deserialize OuterEnum """
        # make sure that an exception is thrown on an invalid value
        with self.assertRaises(petstore_api.ApiValueError):
            self.deserialize(
                MockResponse(data=json.dumps("test str")),
                "OuterEnum"
            )

        # valid value works
        placed_str = (
            petstore_api.OuterEnum.allowed_values[('value',)]["PLACED"]
        )
        response = MockResponse(data=json.dumps(placed_str))
        outer_enum = self.deserialize(response, "OuterEnum")
        self.assertTrue(isinstance(outer_enum, petstore_api.OuterEnum))
        self.assertTrue(outer_enum.value == placed_str)

    def test_deserialize_OuterNumber(self):
        """ deserialize OuterNumber """
        # make sure that an exception is thrown on an invalid type value
        with self.assertRaises(petstore_api.ApiValueError):
            deserialized = self.deserialize(
                MockResponse(data=json.dumps("test str")),
                "OuterNumber"
            )

        # make sure that an exception is thrown on an invalid value
        with self.assertRaises(petstore_api.ApiValueError):
            deserialized = self.deserialize(
                MockResponse(data=json.dumps(21)),
                "OuterNumber"
            )

        # valid value works
        number_val = 11
        response = MockResponse(data=json.dumps(number_val))
        outer_number = self.deserialize(response, "OuterNumber")
        self.assertTrue(isinstance(outer_number, petstore_api.OuterNumber))
        self.assertTrue(outer_number.value == number_val)