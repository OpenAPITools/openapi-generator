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

import SwaggerPetstore
from SwaggerPetstore.rest import ApiException


class StoreApiTests(unittest.TestCase):

    def setUp(self):
        self.store_api = SwaggerPetstore.StoreApi()

    def tearDown(self):
        # sleep 1 sec between two every 2 tests
        time.sleep(1)

    def test_get_inventory(self):
        data = self.store_api.get_inventory()
        self.assertIsNotNone(data)
        self.assertTrue(isinstance(data, dict))
        self.assertItemsEqual(data.keys(), ['available', 'string', 'sold', 'pending', 'confused', 'active', 'na'])
