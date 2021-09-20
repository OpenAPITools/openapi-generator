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
from petstore_api.model import (
    enum_test,
    pet,
    animal,
    dog,
    parent_pet,
    child_lizard,
    category,
    string_enum,
    number_with_validations,
    string_boolean_map,
)
from petstore_api.model_utils import (
    file_type,
    model_to_dict,
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
                "stringEnum": "placed"
            }
        }
        response = MockResponse(data=json.dumps(data))

        deserialized = self.deserialize(response,
            ({str: (enum_test.EnumTest,)},), True)
        self.assertTrue(isinstance(deserialized, dict))
        self.assertTrue(
            isinstance(deserialized['enum_test'], enum_test.EnumTest))
        value = (
            string_enum.StringEnum.allowed_values[('value',)]["PLACED"])
        string_enum_val = string_enum.StringEnum(value)
        sample_instance = enum_test.EnumTest(
            enum_string="UPPER",
            enum_string_required="lower",
            enum_integer=1,
            enum_number=1.1,
            string_enum=string_enum_val
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
                        "fullName": "string"
                    }
                ],
                "status": "available"
            }
        }
        response = MockResponse(data=json.dumps(data))

        deserialized = self.deserialize(response,
            ({str: (pet.Pet,)},), True)
        self.assertTrue(isinstance(deserialized, dict))
        self.assertTrue(isinstance(deserialized['pet'], pet.Pet))

    def test_deserialize_dict_str_dog(self):
        """ deserialize dict(str, Dog), use discriminator
        This will fail because additional_properties_type is None in Animal and it must be defined as any type
        to allow in the property breed which is not defined in Animal, it is defined in Dog
        """
        with self.assertRaises(petstore_api.exceptions.ApiValueError):
            data = {
                'dog': {
                    "className": "Dog",
                    "color": "white",
                    "breed": "Jack Russel Terrier"
                }
            }
            response = MockResponse(data=json.dumps(data))

            deserialized = self.deserialize(response,
                ({str: (animal.Animal,)},), True)

    def test_deserialize_lizard(self):
        """ deserialize ChildLizard, use discriminator
        because additional_properties_type is None in ChildLizardAllOf
        we are unable to use the discriminator
        For this to work correctly, additional_properties_type must allow in any type
        Then in ChildLizardAllOf defines the property pet_type and allows in lovesRocks as an additionalProperty
        """
        with self.assertRaises(petstore_api.exceptions.ApiValueError):
            data = {
                "pet_type": "ChildLizard",
                "lovesRocks": True
            }
            response = MockResponse(data=json.dumps(data))

            lizard = self.deserialize(response,
                (parent_pet.ParentPet,), True)

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
                    "fullName": "string"
                }
            ],
            "status": "available"
        }
        response = MockResponse(data=json.dumps(data))

        deserialized = self.deserialize(response, (pet.Pet,), True)
        self.assertTrue(isinstance(deserialized, pet.Pet))
        self.assertEqual(deserialized.id, 0)
        self.assertEqual(deserialized.name, "doggie")
        self.assertTrue(isinstance(deserialized.category, category.Category))
        self.assertEqual(deserialized.category.name, "string")
        self.assertTrue(isinstance(deserialized.tags, list))
        self.assertEqual(deserialized.tags[0].full_name, "string")

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
                        "fullName": "string"
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
                        "fullName": "string"
                    }
                ],
                "status": "available"
            }]
        response = MockResponse(data=json.dumps(data))

        deserialized = self.deserialize(response,
            ([pet.Pet],), True)
        self.assertTrue(isinstance(deserialized, list))
        self.assertTrue(isinstance(deserialized[0], pet.Pet))
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
                (string_enum.StringEnum,),
                True
            )

        # valid value works
        placed_str = (
            string_enum.StringEnum.allowed_values[('value',)]["PLACED"]
        )
        response = MockResponse(data=json.dumps(placed_str))
        deserialized = self.deserialize(response,
            (string_enum.StringEnum,), True)
        self.assertTrue(isinstance(deserialized, string_enum.StringEnum))
        self.assertTrue(deserialized.value == placed_str)

    def test_deserialize_NumberWithValidations(self):
        """ deserialize NumberWithValidations """
        # make sure that an exception is thrown on an invalid type value
        with self.assertRaises(ApiTypeError):
            deserialized = self.deserialize(
                MockResponse(data=json.dumps("test str")),
                (number_with_validations.NumberWithValidations,),
                True
            )

        # make sure that an exception is thrown on an invalid value
        with self.assertRaises(ApiValueError):
            deserialized = self.deserialize(
                MockResponse(data=json.dumps(21.0)),
                (number_with_validations.NumberWithValidations,),
                True
            )

        # valid value works
        number_val = 11.0
        response = MockResponse(data=json.dumps(number_val))
        number = self.deserialize(response,
            (number_with_validations.NumberWithValidations,), True)
        self.assertTrue(isinstance(number, number_with_validations.NumberWithValidations))
        self.assertTrue(number.value == number_val)

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

    def test_deserialize_binary_to_str(self):
        """Ensures that bytes deserialization works"""
        response_types_mixed = (str,)

        # sample from http://www.jtricks.com/download-text
        HTTPResponse = namedtuple(
            'urllib3_response_HTTPResponse',
            ['status', 'reason', 'data', 'getheaders', 'getheader']
        )
        headers = {}
        def get_headers():
            return headers
        def get_header(name, default=None):
            return headers.get(name, default)
        data = "str"

        http_response = HTTPResponse(
            status=200,
            reason='OK',
            data=json.dumps(data).encode("utf-8") if six.PY3 else json.dumps(data),
            getheaders=get_headers,
            getheader=get_header
        )

        mock_response = RESTResponse(http_response)

        result = self.deserialize(mock_response, response_types_mixed, True)
        self.assertEqual(isinstance(result, str), True)
        self.assertEqual(result, data)

    def test_deserialize_string_boolean_map(self):
        """
        Ensures that string boolean (additional properties)
        deserialization works
        """
        # make sure that an exception is thrown on an invalid type
        with self.assertRaises(ApiTypeError):
            deserialized = self.deserialize(
                MockResponse(data=json.dumps("test str")),
                (string_boolean_map.StringBooleanMap,),
                True
            )

        # valid value works
        item_val = {'some_key': True}
        response = MockResponse(data=json.dumps(item_val))
        model = string_boolean_map.StringBooleanMap(**item_val)
        deserialized = self.deserialize(response,
            (string_boolean_map.StringBooleanMap,), True)
        self.assertTrue(isinstance(deserialized, string_boolean_map.StringBooleanMap))
        self.assertTrue(deserialized['some_key'] == True)
        self.assertTrue(deserialized == model)

