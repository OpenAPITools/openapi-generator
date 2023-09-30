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

import petstore_api
from petstore_api import Configuration
from petstore_api.rest import ApiException

HOST = 'http://localhost/v2'

class StoreApiTests(unittest.TestCase):

    def setUp(self):
        config = Configuration()
        config.host = HOST
        api_client = petstore_api.ApiClient(config)
        self.store_api = petstore_api.StoreApi(api_client)

    def tearDown(self):
        # sleep 1 sec between two every 2 tests
        time.sleep(1)

    def test_get_inventory(self):
        data = self.store_api.get_inventory()
        self.assertIsNotNone(data)
        self.assertTrue(isinstance(data, dict))
