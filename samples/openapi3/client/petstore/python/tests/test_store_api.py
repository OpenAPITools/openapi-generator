# coding: utf-8

# flake8: noqa

"""
Run the tests.
$ pip install -U pytest
$ cd OpenAP/Petstore-python
$ pytest
"""

import os
import time
import unittest

import petstore_client
from petstore_client.rest import ApiException


class StoreApiTests(unittest.TestCase):

    def setUp(self):
        self.store_api = petstore_client.StoreApi()

    def tearDown(self):
        # sleep 1 sec between two every 2 tests
        time.sleep(1)

    def test_get_inventory(self):
        data = self.store_api.get_inventory()
        self.assertIsNotNone(data)
        self.assertTrue(isinstance(data, dict))
