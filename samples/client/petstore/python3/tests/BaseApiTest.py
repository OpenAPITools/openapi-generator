#!/usr/bin/env python
"""Unit tests for SwaggerPython Petstore sample app API client.

Run all tests:

    python BaseApiTest.py

"""

import sys
import os
import unittest

sys.path = ['./'] + sys.path
from petstore import *


class BaseApiTest(unittest.TestCase):

    def setUp(self):
        self.apiKey = 'special-key'
        self.apiUrl = 'http://petstore.swagger.wordnik.com/api'
        self.username = 'test'
        self.password = 'test'

        client = swagger.ApiClient(self.apiKey, self.apiUrl)
        self.petApi = PetApi.PetApi(client)
        self.storeApi = StoreApi.StoreApi(client)
        self.userApi = UserApi.UserApi(client)

if __name__ == "__main__":

    from PetApiTest import PetApiTest
    from StoreApiTest import StoreApiTest
    from UserApiTest import UserApiTest

    unittest.main()
