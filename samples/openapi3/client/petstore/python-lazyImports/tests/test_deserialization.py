# coding: utf-8

# flake8: noqa

"""
Run the tests.
$ pip install -U pytest
$ cd OpenAPIPetstore-python
$ pytest
"""
from collections import namedtuple
import json
import os
import time
import unittest
import datetime
from decimal import Decimal

import pytest as pytest

import petstore_api


class DeserializationTests(unittest.TestCase):

    def setUp(self):
        self.api_client = petstore_api.ApiClient()
        self.deserialize = self.api_client.deserialize

    def test_enum_test(self):
       """ deserialize Dict[str, EnumTest] """
       data = {
           'enum_test': {
               "enum_string": "UPPER",
               "enum_string_required": "lower",
               "enum_integer": 1,
               "enum_number": 1.1,
               "outerEnum": "placed"
           }
       }
       response = json.dumps(data)

       deserialized = self.deserialize(response, 'Dict[str, EnumTest]', 'application/json')
       self.assertTrue(isinstance(deserialized, dict))
       self.assertTrue(isinstance(deserialized['enum_test'], petstore_api.EnumTest))
       self.assertEqual(deserialized['enum_test'],
                        petstore_api.EnumTest(enum_string="UPPER",
                                              enum_string_required="lower",
                                              enum_integer=1,
                                              enum_number=1.1,
                                              outerEnum=petstore_api.OuterEnum.PLACED))

    def test_deserialize_dict_str_pet(self):
        """ deserialize Dict[str, Pet] """
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
        response = json.dumps(data)

        deserialized = self.deserialize(response, 'Dict[str, Pet]', 'application/json')
        self.assertTrue(isinstance(deserialized, dict))
        self.assertTrue(isinstance(deserialized['pet'], petstore_api.Pet))

    @pytest.mark.skip(reason="skipping for now as deserialization will be refactored")
    def test_deserialize_dict_str_dog(self):
        """ deserialize Dict[str, Animal], use discriminator"""
        data = {
            'dog': {
                "id": 0,
                "className": "Dog",
                "color": "white",
                "bread": "Jack Russel Terrier"
            }
        }
        response = json.dumps(data)

        deserialized = self.deserialize(response, 'Dict[str, Animal]', 'application/json')
        self.assertTrue(isinstance(deserialized, dict))
        self.assertTrue(isinstance(deserialized['dog'], petstore_api.Dog))

    @pytest.mark.skip(reason="skipping for now as deserialization will be refactored")
    def test_deserialize_dict_str_int(self):
        """ deserialize Dict[str, int] """
        data = {
            'integer': 1
        }
        response = json.dumps(data)

        deserialized = self.deserialize(response, 'Dict[str, int]', 'application/json')
        self.assertTrue(isinstance(deserialized, dict))
        self.assertTrue(isinstance(deserialized['integer'], int))

    def test_deserialize_str(self):
        """ deserialize str """
        data = "test str"
        response = data=json.dumps(data)

        deserialized = self.deserialize(response, "str", 'application/json')
        self.assertTrue(isinstance(deserialized, str))

    def test_deserialize_date(self):
        """ deserialize date """
        data = "1997-07-16"
        response = data=json.dumps(data)

        deserialized = self.deserialize(response, "date", 'application/json')
        self.assertTrue(isinstance(deserialized, datetime.date))

    def test_deserialize_datetime(self):
        """ deserialize datetime """
        data = "1997-07-16T19:20:30.45+01:00"
        response = json.dumps(data)

        deserialized = self.deserialize(response, "datetime", 'application/json')
        self.assertTrue(isinstance(deserialized, datetime.datetime))

    def test_deserialize_decimal(self):
        """ deserialize decimal """
        data = 1.1
        response = json.dumps(data)

        deserialized = self.deserialize(response, "decimal", 'application/json')
        self.assertTrue(isinstance(deserialized, Decimal))
        self.assertEqual(deserialized, Decimal(1.1))

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
        response = json.dumps(data)

        deserialized = self.deserialize(response, "Pet", 'application/json')
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
        response = json.dumps(data)

        deserialized = self.deserialize(response, "List[Pet]", 'application/json')
        self.assertTrue(isinstance(deserialized, list))
        self.assertTrue(isinstance(deserialized[0], petstore_api.Pet))
        self.assertEqual(deserialized[0].id, 0)
        self.assertEqual(deserialized[1].id, 1)
        self.assertEqual(deserialized[0].name, "doggie0")
        self.assertEqual(deserialized[1].name, "doggie1")

    def test_deserialize_nested_dict(self):
        """ deserialize Dict[str, Dict[str, int]] """
        data = {
            "foo": {
                "bar": 1
            }
        }
        response = json.dumps(data)

        deserialized = self.deserialize(response, "Dict[str, Dict[str, int]]", 'application/json')
        self.assertTrue(isinstance(deserialized, dict))
        self.assertTrue(isinstance(deserialized["foo"], dict))
        self.assertTrue(isinstance(deserialized["foo"]["bar"], int))

    def test_deserialize_nested_list(self):
        """ deserialize list[list[str]] """
        data = [["foo"]]
        response = json.dumps(data)

        deserialized = self.deserialize(response, "List[List[str]]", 'application/json')
        self.assertTrue(isinstance(deserialized, list))
        self.assertTrue(isinstance(deserialized[0], list))
        self.assertTrue(isinstance(deserialized[0][0], str))

    def test_deserialize_none(self):
        """ deserialize None """
        response = json.dumps(None)

        deserialized = self.deserialize(response, "datetime", 'application/json')
        self.assertIsNone(deserialized)

    def test_deserialize_pig(self):
        """ deserialize pig (oneOf) """
        data = {
            "className": "BasqueBig",
            "color": "white"
        }

        response = json.dumps(data)
        deserialized = self.deserialize(response, "Pig", 'application/json')
        self.assertTrue(isinstance(deserialized.actual_instance,
                                   petstore_api.BasquePig))
        self.assertEqual(deserialized.actual_instance.class_name, "BasqueBig")
        self.assertEqual(deserialized.actual_instance.color, "white")

    def test_deserialize_animal(self):
        """ deserialize animal with discriminator mapping """
        data = {
            "declawed": True,
            "className": "Cat2222"  # incorrect class name
        }

        response = json.dumps(data)

        with pytest.raises(ValueError) as ex:
            deserialized = self.deserialize(response, "Animal", 'application/json')
        assert str(
            ex.value) == 'Animal failed to lookup discriminator value from {"declawed": true, "className": ' \
                         '"Cat2222"}. Discriminator property name: className, mapping: {"Cat": "Cat", "Dog": "Dog"}'

        data = {
            "declawed": True,
            "className": "Cat"  # correct class name
        }

        response = json.dumps(data)

        deserialized = self.deserialize(response, "Animal", 'application/json')
        self.assertTrue(isinstance(deserialized, petstore_api.Cat))
        self.assertEqual(deserialized.class_name, "Cat")
        self.assertEqual(deserialized.declawed, True)
        self.assertEqual(deserialized.to_json(), '{"className": "Cat", "color": "red", "declawed": true}')

        # test from json
        json_str = '{"className": "Cat", "color": "red", "declawed": true}'

        deserialized = petstore_api.Animal.from_json(json_str)

        # the following is necessary for mypy as it does not handle isinstance() within self.assertTrue() well
        if not isinstance(deserialized, petstore_api.Cat):
            self.assertTrue(False)
            return

        self.assertEqual(deserialized.class_name, "Cat")
        self.assertEqual(deserialized.declawed, True)
        self.assertEqual(deserialized.to_json(), '{"className": "Cat", "color": "red", "declawed": true}')

    def test_deserialize_content_type(self):

        response = json.dumps({"a": "a"})
        
        deserialized = self.deserialize(response, "Dict[str, str]", 'application/json')
        self.assertTrue(isinstance(deserialized, dict))

        
        deserialized = self.deserialize(response, "Dict[str, str]", 'application/vnd.api+json')
        self.assertTrue(isinstance(deserialized, dict))


        deserialized = self.deserialize(response, "Dict[str, str]", 'application/json; charset=utf-8')
        self.assertTrue(isinstance(deserialized, dict))

        deserialized = self.deserialize(response, "Dict[str, str]", 'application/vnd.api+json; charset=utf-8')
        self.assertTrue(isinstance(deserialized, dict))

        deserialized = self.deserialize(response, "str", 'text/plain')
        self.assertTrue(isinstance(deserialized, str))

        deserialized = self.deserialize(response, "str", 'text/csv')
        self.assertTrue(isinstance(deserialized, str))

        deserialized = self.deserialize(response, "Dict[str, str]", 'APPLICATION/JSON')
        self.assertTrue(isinstance(deserialized, dict))

        with self.assertRaises(petstore_api.ApiException) as cm:
            deserialized = self.deserialize(response, "str", 'text')

        with self.assertRaises(petstore_api.ApiException) as cm:
            deserialized = self.deserialize(response, "str", 'text/n0t-exist!ng')

        with self.assertRaises(petstore_api.ApiException) as cm:
            deserialized = self.deserialize(response, "Dict[str, str]", 'application/jsonnnnn')

        with self.assertRaises(petstore_api.ApiException) as cm:
            deserialized = self.deserialize(response, "Dict[str, str]", 'application/<+json')

