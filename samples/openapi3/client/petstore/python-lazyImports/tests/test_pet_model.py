# coding: utf-8

# flake8: noqa

import os
import time
import unittest

import petstore_api
import json
from pydantic import ValidationError


class PetModelTests(unittest.TestCase):

    def setUp(self):
        self.pet = petstore_api.Pet(name="test name", photoUrls=["string"])
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
        data = ("{'additional_properties': {},\n"
                " 'category': {'additional_properties': {}, 'id': 1, 'name': 'dog'},\n"
                " 'id': 1,\n"
                " 'name': 'test name',\n"
                " 'photoUrls': ['string'],\n"
                " 'status': 'available',\n"
                " 'tags': [{'additional_properties': {}, 'id': 1, 'name': None}]}")
        self.assertEqual(data, self.pet.to_str())

    def test_equal(self):
        self.pet1 = petstore_api.Pet(name="test name", photoUrls=["string"])
        self.pet1.id = 1
        self.pet1.status = "available"
        cate1 = petstore_api.Category(name="dog")
        cate1.id = 1
        # cate1.name = "dog"
        self.pet1.category = cate1
        tag1 = petstore_api.Tag()
        tag1.id = 1
        self.pet1.tags = [tag1]

        self.pet2 = petstore_api.Pet(name="test name", photoUrls=["string"])
        self.pet2.id = 1
        self.pet2.status = "available"
        cate2 = petstore_api.Category(name="dog")
        cate2.id = 1
        # cate2.name = "dog"
        self.pet2.category = cate2
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
        assert pet is not None
        self.assertEqual(pet.id, 1)
        self.assertEqual(pet.status, "available")
        self.assertEqual(pet.photo_urls, ["string"])
        assert pet.tags is not None
        self.assertEqual(pet.tags[0].id, 1)
        self.assertEqual(pet.tags[0].name, "None")
        assert pet.category is not None
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
        assert pet2 is not None
        self.assertEqual(pet2.id, 1)
        self.assertEqual(pet2.status, "available")
        self.assertEqual(pet2.photo_urls, ["string"])
        assert pet2.tags is not None
        self.assertEqual(pet2.tags[0].id, 1)
        self.assertEqual(pet2.tags[0].name, "None")
        assert pet2.category is not None
        self.assertEqual(pet2.category.id, 1)

    def test_unpack_operator(self):
        pet = petstore_api.Pet(name="required name", id=123, photoUrls=["https://a.com", "https://b.com"])
        self.assertEqual(pet.to_json(), '{"id": 123, "name": "required name", "photoUrls": ["https://a.com", "https://b.com"]}')
        self.assertEqual(pet.to_dict(), {"id": 123, "name": "required name", "photoUrls": ["https://a.com", "https://b.com"]})

    def test_optional_fields(self):
        _pet = petstore_api.Pet(name="required name",
                               photoUrls=["https://a.com",
                                          "https://b.com"])
        self.assertEqual(_pet.to_json(),'{"name": "required name", "photoUrls": ["https://a.com", "https://b.com"]}')
        self.assertEqual(_pet.to_dict(), {"name": "required name", "photoUrls": ["https://a.com", "https://b.com"]})



