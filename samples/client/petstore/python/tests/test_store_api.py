# coding: utf-8

"""
Run the tests.
$ pip install nose (optional)
$ cd SwaggerPetstore-python
$ nosetests -v
"""

import os
import time
import unittest

import swagger_client
from swagger_client.rest import ApiException


class StoreApiTests(unittest.TestCase):

    def setUp(self):
        self.store_api = swagger_client.StoreApi()

    def tearDown(self):
        # sleep 1 sec between two every 2 tests
        time.sleep(1)

    def test_get_inventory(self):
        data = self.store_api.get_inventory()
        self.assertIsNotNone(data)
        self.assertTrue(isinstance(data, dict))
