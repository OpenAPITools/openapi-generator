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
