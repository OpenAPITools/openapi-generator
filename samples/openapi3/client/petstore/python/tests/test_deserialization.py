# coding: utf-8

# flake8: noqa

"""
Run the tests.
$ pip install -U pytest
$ cd OpenAPIPetstore-python
$ pytest
"""
from collections import namedtuple
from unittest.mock import Mock
import json
import unittest
import datetime

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
       deserialized = self.deserialize(Mock(), 'Dict[str, EnumTest]', body=json.dumps(data))
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
        deserialized = self.deserialize(Mock(), 'Dict[str, Pet]', body=json.dumps(data))
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
        deserialized = self.deserialize(Mock(), 'Dict[str, Animal]', body=json.dumps(data))
        self.assertTrue(isinstance(deserialized, dict))
        self.assertTrue(isinstance(deserialized['dog'], petstore_api.Dog))

    @pytest.mark.skip(reason="skipping for now as deserialization will be refactored")
    def test_deserialize_dict_str_int(self):
        """ deserialize Dict[str, int] """
        data = {
            'integer': 1
        }
        deserialized = self.deserialize(Mock(), 'Dict[str, int]', body=json.dumps(data))
        self.assertTrue(isinstance(deserialized, dict))
        self.assertTrue(isinstance(deserialized['integer'], int))

    def test_deserialize_str(self):
        """ deserialize str """
        data = "test str"
        deserialized = self.deserialize(Mock(), "str", body=json.dumps(data))
        self.assertTrue(isinstance(deserialized, str))

    def test_deserialize_date(self):
        """ deserialize date """
        data = "1997-07-16"
        deserialized = self.deserialize(Mock(), "date", body=json.dumps(data))
        self.assertTrue(isinstance(deserialized, datetime.date))

    def test_deserialize_datetime(self):
        """ deserialize datetime """
        data = "1997-07-16T19:20:30.45+01:00"
        deserialized = self.deserialize(Mock(), "datetime", body=json.dumps(data))
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
        deserialized = self.deserialize(Mock(), "Pet", body=json.dumps(data))
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
        deserialized = self.deserialize(Mock(), "List[Pet]", body=json.dumps(data))
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
        deserialized = self.deserialize(Mock(), "Dict[str, Dict[str, int]]", body=json.dumps(data))
        self.assertTrue(isinstance(deserialized, dict))
        self.assertTrue(isinstance(deserialized["foo"], dict))
        self.assertTrue(isinstance(deserialized["foo"]["bar"], int))

    def test_deserialize_nested_list(self):
        """ deserialize list[list[str]] """
        data = [["foo"]]
        deserialized = self.deserialize(Mock(), "List[List[str]]", body=json.dumps(data))
        self.assertTrue(isinstance(deserialized, list))
        self.assertTrue(isinstance(deserialized[0], list))
        self.assertTrue(isinstance(deserialized[0][0], str))

    def test_deserialize_none(self):
        """ deserialize None """
        data = None
        deserialized = self.deserialize(Mock(), "datetime", body=json.dumps(data))
        self.assertIsNone(deserialized)

    def test_deserialize_pig(self):
        """ deserialize pig (oneOf) """
        data = {
            "className": "BasqueBig",
            "color": "white"
        }
        deserialized = self.deserialize(Mock(), "Pig", body=json.dumps(data))
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
        with pytest.raises(ValueError) as ex:
            self.deserialize(Mock(), "Animal", body=json.dumps(data))
        assert str(
            ex.value) == 'Animal failed to lookup discriminator value from {"declawed": true, "className": ' \
                         '"Cat2222"}. Discriminator property name: className, mapping: {"Cat": "Cat", "Dog": "Dog"}'

        data = {
            "declawed": True,
            "className": "Cat"  # correct class name
        }
        deserialized = self.deserialize(Mock(), "Animal", body=json.dumps(data))
        self.assertTrue(isinstance(deserialized, petstore_api.Cat))
        self.assertEqual(deserialized.class_name, "Cat")
        self.assertEqual(deserialized.declawed, True)
        self.assertEqual(deserialized.to_json(), '{"className": "Cat", "color": "red", "declawed": true}')

        # test from json
        json_str = '{"className": "Cat", "color": "red", "declawed": true}'

        deserialized = petstore_api.Animal.from_json(json_str)
        self.assertTrue(isinstance(deserialized, petstore_api.Cat))
        self.assertEqual(deserialized.class_name, "Cat")
        self.assertEqual(deserialized.declawed, True)
        self.assertEqual(deserialized.to_json(), '{"className": "Cat", "color": "red", "declawed": true}')
