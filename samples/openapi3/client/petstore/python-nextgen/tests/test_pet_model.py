# coding: utf-8

# flake8: noqa

"""
Run the tests.
$ pip install nose (optional)
$ cd petstore_api-python
$ nosetests -v
"""

import os
import time
import unittest

import petstore_api
from pydantic import ValidationError


class PetModelTests(unittest.TestCase):

    def setUp(self):
        self.pet = petstore_api.Pet(name="test name", photo_urls=["string"])
        self.pet.id = 1
        self.pet.status = "available"
        cate = petstore_api.Category(name="dog")
        cate.id = 1
        # cate.name = "dog"
        self.pet.category = cate
        tag = petstore_api.Tag()
        tag.id = 1
        self.pet.tags = [tag]

    def test_to_str(self):
        data = ("{'category': {'id': 1, 'name': 'dog'},\n"
                " 'id': 1,\n"
                " 'name': 'test name',\n"
                " 'photoUrls': ['string'],\n"
                " 'status': 'available',\n"
                " 'tags': [{'id': 1}]}")
        self.assertEqual(data, self.pet.to_str())

    def test_equal(self):
        self.pet1 = petstore_api.Pet(name="test name", photo_urls=["string"])
        self.pet1.id = 1
        self.pet1.status = "available"
        cate1 = petstore_api.Category(name="dog")
        cate1.id = 1
        # cate1.name = "dog"
        self.pet.category = cate1
        tag1 = petstore_api.Tag()
        tag1.id = 1
        self.pet1.tags = [tag1]

        self.pet2 = petstore_api.Pet(name="test name", photo_urls=["string"])
        self.pet2.id = 1
        self.pet2.status = "available"
        cate2 = petstore_api.Category(name="dog")
        cate2.id = 1
        # cate2.name = "dog"
        self.pet.category = cate2
        tag2 = petstore_api.Tag()
        tag2.id = 1
        self.pet2.tags = [tag2]

        self.assertTrue(self.pet1 == self.pet2)

        # reset pet1 tags to empty array so that object comparison returns false
        self.pet1.tags = []
        self.assertFalse(self.pet1 == self.pet2)

    # test from_json, to_json, to_dict, from_dict
    def test_from_to_methods(self):
        json_str = ("{\"category\": {\"id\": 1, \"name\": \"dog\"},\n"
                    " \"id\": 1,\n"
                    " \"name\": \"test name\",\n"
                    " \"photoUrls\": [\"string\"],\n"
                    " \"status\": \"available\",\n"
                    " \"tags\": [{\"id\": 1, \"name\": \"None\"}]}")
        pet = petstore_api.Pet.from_json(json_str)
        self.assertEqual(pet.id, 1)
        self.assertEqual(pet.status, "available")
        self.assertEqual(pet.photo_urls, ["string"])
        self.assertEqual(pet.tags[0].id, 1)
        self.assertEqual(pet.tags[0].name, "None")
        self.assertEqual(pet.category.id, 1)
        # test to_json
        self.assertEqual(pet.to_json(),
                         '{"id": 1, "category": {"id": 1, "name": "dog"}, "name": "test name", "photoUrls": ['
                         '"string"], "tags": [{"id": 1, "name": "None"}], "status": "available"}')

        # test to_dict
        self.assertEqual(pet.to_dict(),
                         {"id": 1, "category": {"id": 1, "name": "dog"}, "name": "test name", "photoUrls": ["string"],
                          "tags": [{"id": 1, "name": "None"}], "status": "available"})

        # test from_dict
        pet2 = petstore_api.Pet.from_dict(pet.to_dict())
        self.assertEqual(pet2.id, 1)
        self.assertEqual(pet2.status, "available")
        self.assertEqual(pet2.photo_urls, ["string"])
        self.assertEqual(pet2.tags[0].id, 1)
        self.assertEqual(pet2.tags[0].name, "None")
        self.assertEqual(pet2.category.id, 1)

    def test_optional_fields(self):
        pet = petstore_api.Pet(name="required name",
                               photoUrls=["https://a.com",
                                          "https://b.com"])
        self.assertEqual(pet.to_json(), '{"name": "required name", "photoUrls": ["https://a.com", "https://b.com"]}')
        self.assertEqual(pet.to_dict(), {"name": "required name", "photoUrls": ["https://a.com", "https://b.com"]})

    def test_inheritance(self):
        dog = petstore_api.Dog(breed="bulldog", className="dog", color="white")
        self.assertEqual(dog.to_json(), '{"className": "dog", "color": "white", "breed": "bulldog"}')
        self.assertEqual(dog.to_dict(), {'breed': 'bulldog', 'className':
            'dog', 'color': 'white'})
        dog2 = petstore_api.Dog.from_json(dog.to_json())
        self.assertEqual(dog2.breed, 'bulldog')
        self.assertEqual(dog2.class_name, "dog")
        self.assertEqual(dog2.color, 'white')

        self.assertTrue(isinstance(dog2, petstore_api.Animal))

    def test_oneOf(self):
        # succeeded
        json_str = '{"className": "BasquePig", "color": "red"}'
        p = petstore_api.Pig.from_json(json_str)
        self.assertIsInstance(p.actual_instance, petstore_api.BasquePig)

        # test init
        basque_pig = p.actual_instance
        pig2 = petstore_api.Pig(actual_instance=basque_pig)
        self.assertIsInstance(pig2.actual_instance, petstore_api.BasquePig)

        # test failed init
        try:
            pig3 = petstore_api.Pig(actual_instance="123")
            self.assertTrue(False)  # this line shouldn't execute
        except ValueError as e:
            self.assertTrue(
                "No match found when deserializing the JSON string into Pig with oneOf schemas: BasquePig, DanishPig" in str(
                    e))

        # failure
        try:
            p2 = petstore_api.Pig.from_json("1")
            self.assertTrue(False)  # this line shouldn't execute
        except ValueError as e:
            error_message = (
                "No match found when deserializing the JSON string into Pig with oneOf schemas: BasquePig, DanishPig. "
                "Details: 1 validation error for BasquePig\n "
                "__root__\n"
                "  BasquePig expected dict not int (type=type_error), 1 validation error for DanishPig\n"
                "__root__\n"
                "  DanishPig expected dict not int (type=type_error)")
            self.assertEqual(str(e), error_message)

        # test to_json
        self.assertEqual(p.to_json(), '{"className": "BasquePig", "color": "red"}')

    def test_anyOf(self):
        # succeeded
        json_str = '{"className": "BasquePig", "color": "red"}'
        p = petstore_api.AnyOfPig.from_json(json_str)
        self.assertIsInstance(p.actual_instance, petstore_api.BasquePig)

        # test init
        basque_pig = p.actual_instance
        pig2 = petstore_api.Pig(actual_instance=basque_pig)
        self.assertIsInstance(pig2.actual_instance, petstore_api.BasquePig)

        # test failed init
        try:
            pig3 = petstore_api.AnyOfPig(actual_instance="123")
            self.assertTrue(False)  # this line shouldn't execute
        except ValueError as e:
            self.assertTrue(
                "No match found when deserializing the JSON string into AnyOfPig with anyOf schemas: BasquePig, "
                "DanishPig" in str(
                    e))

        # failure
        try:
            p2 = petstore_api.AnyOfPig.from_json("1")
            self.assertTrue(False)  # this line shouldn't execute
        except ValueError as e:
            error_message = (
                "No match found when deserializing the JSON string into AnyOfPig with anyOf schemas: BasquePig, "
                "DanishPig. Details: 1 validation error for BasquePig\n "
                "__root__\n"
                "  BasquePig expected dict not int (type=type_error), 1 validation error for DanishPig\n"
                "__root__\n"
                "  DanishPig expected dict not int (type=type_error)")
            self.assertEqual(str(e), error_message)

        # test to_json
        self.assertEqual(p.to_json(), '{"className": "BasquePig", "color": "red"}')
