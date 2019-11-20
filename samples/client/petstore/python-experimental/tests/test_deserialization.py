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

import six

import petstore_api

from petstore_api.exceptions import (
    ApiTypeError,
    ApiKeyError,
    ApiValueError,
)

from petstore_api.model_utils import (
    file_type,
    int,
    str,
)

from petstore_api.rest import RESTResponse

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

        deserialized = self.deserialize(response,
            ({str: (petstore_api.EnumTest,)},), True)
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

        deserialized = self.deserialize(response,
            ({str: (petstore_api.Pet,)},), True)
        self.assertTrue(isinstance(deserialized, dict))
        self.assertTrue(isinstance(deserialized['pet'], petstore_api.Pet))

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

        deserialized = self.deserialize(response,
            ({str: (petstore_api.Animal,)},), True)
        self.assertTrue(isinstance(deserialized, dict))
        self.assertTrue(isinstance(deserialized['dog'], petstore_api.Dog))

    def test_deserialize_dict_str_int(self):
        """ deserialize dict(str, int) """
        data = {
            'integer': 1
        }
        response = MockResponse(data=json.dumps(data))

        deserialized = self.deserialize(response, ({str: (int,)},), True)
        self.assertTrue(isinstance(deserialized, dict))
        self.assertTrue(isinstance(deserialized['integer'], int))

    def test_deserialize_str(self):
        """ deserialize str """
        data = "test str"
        response = MockResponse(data=json.dumps(data))

        deserialized = self.deserialize(response, (str,), True)
        self.assertTrue(isinstance(deserialized, str))

    def test_deserialize_date(self):
        """ deserialize date """
        data = "1997-07-16"
        response = MockResponse(data=json.dumps(data))

        deserialized = self.deserialize(response, (datetime.date,), True)
        self.assertTrue(isinstance(deserialized, datetime.date))

    def test_deserialize_datetime(self):
        """ deserialize datetime """
        data = "1997-07-16T19:20:30.45+01:00"
        response = MockResponse(data=json.dumps(data))

        deserialized = self.deserialize(response, (datetime.datetime,), True)
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

        deserialized = self.deserialize(response, (petstore_api.Pet,), True)
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

        deserialized = self.deserialize(response,
            ([petstore_api.Pet],), True)
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

        deserialized = self.deserialize(response,
            ({str: ({str: (int,)},)},), True)
        self.assertTrue(isinstance(deserialized, dict))
        self.assertTrue(isinstance(deserialized["foo"], dict))
        self.assertTrue(isinstance(deserialized["foo"]["bar"], int))

    def test_deserialize_nested_list(self):
        """ deserialize list[list[str]] """
        data = [["foo"]]
        response = MockResponse(data=json.dumps(data))

        deserialized = self.deserialize(response, ([[str]],), True)
        self.assertTrue(isinstance(deserialized, list))
        self.assertTrue(isinstance(deserialized[0], list))
        self.assertTrue(isinstance(deserialized[0][0], str))

    def test_deserialize_none(self):
        """ deserialize None """
        response = MockResponse(data=json.dumps(None))

        error_msg = (
            "Invalid type for variable 'received_data'. Required value type is "
            "datetime and passed type was NoneType at ['received_data']"
        )
        with self.assertRaises(ApiTypeError) as exc:
            deserialized = self.deserialize(response, (datetime.datetime,), True)
        self.assertEqual(str(exc.exception), error_msg)

    def test_deserialize_OuterEnum(self):
        """ deserialize OuterEnum """
        # make sure that an exception is thrown on an invalid value
        with self.assertRaises(ApiValueError):
            self.deserialize(
                MockResponse(data=json.dumps("test str")),
                (petstore_api.OuterEnum,),
                True
            )

        # valid value works
        placed_str = (
            petstore_api.OuterEnum.allowed_values[('value',)]["PLACED"]
        )
        response = MockResponse(data=json.dumps(placed_str))
        outer_enum = self.deserialize(response,
            (petstore_api.OuterEnum,), True)
        self.assertTrue(isinstance(outer_enum, petstore_api.OuterEnum))
        self.assertTrue(outer_enum.value == placed_str)

    def test_deserialize_OuterNumber(self):
        """ deserialize OuterNumber """
        # make sure that an exception is thrown on an invalid type value
        with self.assertRaises(ApiTypeError):
            deserialized = self.deserialize(
                MockResponse(data=json.dumps("test str")),
                (petstore_api.OuterNumber,),
                True
            )

        # make sure that an exception is thrown on an invalid value
        with self.assertRaises(ApiValueError):
            deserialized = self.deserialize(
                MockResponse(data=json.dumps(21.0)),
                (petstore_api.OuterNumber,),
                True
            )

        # valid value works
        number_val = 11.0
        response = MockResponse(data=json.dumps(number_val))
        outer_number = self.deserialize(response,
            (petstore_api.OuterNumber,), True)
        self.assertTrue(isinstance(outer_number, petstore_api.OuterNumber))
        self.assertTrue(outer_number.value == number_val)

    def test_deserialize_file(self):
        """Ensures that file deserialization works"""
        response_types_mixed = (file_type,)

        # sample from http://www.jtricks.com/download-text
        HTTPResponse = namedtuple(
            'urllib3_response_HTTPResponse',
            ['status', 'reason', 'data', 'getheaders', 'getheader']
        )
        headers = {'Content-Disposition': 'attachment; filename=content.txt'}
        def get_headers():
            return headers
        def get_header(name, default=None):
            return headers.get(name, default)
        file_data = (
            "You are reading text file that was supposed to be downloaded\r\n"
            "to your hard disk. If your browser offered to save you the file,"
            "\r\nthen it handled the Content-Disposition header correctly."
        )
        http_response = HTTPResponse(
            status=200,
            reason='OK',
            data=file_data,
            getheaders=get_headers,
            getheader=get_header
        )
        # response which is deserialized to a file
        mock_response = RESTResponse(http_response)
        file_path = None
        try:
            file_object = self.deserialize(
                mock_response, response_types_mixed, True)
            self.assertTrue(isinstance(file_object, file_type))
            file_path = file_object.name
            self.assertFalse(file_object.closed)
            file_object.close()
            if six.PY3:
                file_data = file_data.encode('utf-8')
            with open(file_path, 'rb') as other_file_object:
                self.assertEqual(other_file_object.read(), file_data)
        finally:
            os.unlink(file_path)

    def test_deserialize_string_boolean_map(self):
        """
        Ensures that string boolean (additional properties)
        deserialization works
        """
        # make sure that an exception is thrown on an invalid type
        with self.assertRaises(ApiTypeError):
            deserialized = self.deserialize(
                MockResponse(data=json.dumps("test str")),
                (petstore_api.StringBooleanMap,),
                True
            )

        # valid value works
        item_val = {'some_key': True}
        response = MockResponse(data=json.dumps(item_val))
        model = petstore_api.StringBooleanMap(**item_val)
        deserialized = self.deserialize(response,
            (petstore_api.StringBooleanMap,), True)
        self.assertTrue(isinstance(deserialized, petstore_api.StringBooleanMap))
        self.assertTrue(deserialized['some_key'] == True)
        self.assertTrue(deserialized == model)

