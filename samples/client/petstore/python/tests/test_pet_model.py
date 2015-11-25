# coding: utf-8

"""
Run the tests.
$ pip install nose (optional)
$ cd swagger_client-python
$ nosetests -v
"""

import os
import time
import unittest

import swagger_client


class PetModelTests(unittest.TestCase):

    def setUp(self):
        self.pet = swagger_client.Pet()
        self.pet.name = "test name"
        self.pet.id = 1
        self.pet.photo_urls = ["string"]
        self.pet.status = "available"
        cate = swagger_client.Category()
        cate.id = 1
        cate.name = "dog"
        self.pet.category = cate
        tag = swagger_client.Tag()
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
        self.pet1 = swagger_client.Pet()
        self.pet1.name = "test name"
        self.pet1.id = 1
        self.pet1.photo_urls = ["string"]
        self.pet1.status = "available"
        cate1 = swagger_client.Category()
        cate1.id = 1
        cate1.name = "dog"
        self.pet.category = cate1
        tag1 = swagger_client.Tag()
        tag1.id = 1
        self.pet1.tags = [tag1]

        self.pet2 = swagger_client.Pet()
        self.pet2.name = "test name"
        self.pet2.id = 1
        self.pet2.photo_urls = ["string"]
        self.pet2.status = "available"
        cate2 = swagger_client.Category()
        cate2.id = 1
        cate2.name = "dog"
        self.pet.category = cate2
        tag2 = swagger_client.Tag()
        tag2.id = 1
        self.pet2.tags = [tag2]

        self.assertTrue(self.pet1 == self.pet2)

        # reset pet1 tags to empty array so that object comparison returns false
        self.pet1.tags = []
        self.assertFalse(self.pet1 == self.pet2)
