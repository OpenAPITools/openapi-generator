# coding: utf-8

# flake8: noqa

"""
Run the tests.
$ pip install nose (optional)
$ cd OpenAP/Petstore-python
$ nosetests -v
"""

import os
import time
import unittest

import petstore_api
from petstore_api.api.store_api import StoreApi


class StoreApiTests(unittest.TestCase):

    def setUp(self):
        self.store_api = StoreApi()

    def tearDown(self):
        # sleep 1 sec between two every 2 tests
        time.sleep(1)

    def test_get_inventory(self):
        data = self.store_api.get_inventory()
        self.assertIsNotNone(data)
        self.assertTrue(isinstance(data, dict))
