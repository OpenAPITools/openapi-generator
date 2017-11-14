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


class PetModelTests(unittest.TestCase):

    def setUp(self):
        self.pet = petstore_api.Pet(name="test name", photo_urls=["string"])
        self.pet.id = 1
        self.pet.status = "available"
        cate = petstore_api.Category()
        cate.id = 1
        cate.name = "dog"
        self.pet.category = cate
        tag = petstore_api.Tag()
        tag.id = 1
        self.pet.tags = [tag]

    def test_to_str(self):
        data = ("{'category': {'id': 1, 'name': 'dog'},\n"
                " 'id': 1,\n"
                " 'name': 'test name',\n"
                " 'photo_urls': ['string'],\n"
                " 'status': 'available',\n"
                " 'tags': [{'id': 1, 'name': None}]}")
        self.assertEqual(data, self.pet.to_str())

    def test_equal(self):
        self.pet1 = petstore_api.Pet(name="test name", photo_urls=["string"])
        self.pet1.id = 1
        self.pet1.status = "available"
        cate1 = petstore_api.Category()
        cate1.id = 1
        cate1.name = "dog"
        self.pet.category = cate1
        tag1 = petstore_api.Tag()
        tag1.id = 1
        self.pet1.tags = [tag1]

        self.pet2 = petstore_api.Pet(name="test name", photo_urls=["string"])
        self.pet2.id = 1
        self.pet2.status = "available"
        cate2 = petstore_api.Category()
        cate2.id = 1
        cate2.name = "dog"
        self.pet.category = cate2
        tag2 = petstore_api.Tag()
        tag2.id = 1
        self.pet2.tags = [tag2]

        self.assertTrue(self.pet1 == self.pet2)

        # reset pet1 tags to empty array so that object comparison returns false
        self.pet1.tags = []
        self.assertFalse(self.pet1 == self.pet2)
