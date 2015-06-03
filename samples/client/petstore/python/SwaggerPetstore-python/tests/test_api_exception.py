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


class ApiExceptionTests(unittest.TestCase):

    def setUp(self):
        self.api_client = SwaggerPetstore.ApiClient()
        self.pet_api = SwaggerPetstore.PetApi(self.api_client)

    def tearDown(self):
        time.sleep(1)

    def test_404_error(self):
        self.pet_api.delete_pet(pet_id=1234)
        
        with self.assertRaisesRegexp(ApiException, "Pet not found"):
            self.pet_api.get_pet_by_id(pet_id=1234)
