# coding: utf-8

# flake8: noqa

"""
Run the tests.
$ pip install -U pytest
$ cd petstore_api-python
$ pytest
"""

import os
import sys
import unittest

import petstore_api
from petstore_api import Configuration
from petstore_api.rest import ApiException

from .util import id_gen

HOST = 'http://localhost/v2'

class ApiExceptionTests(unittest.TestCase):

    def setUp(self):
        config = Configuration()
        config.host = HOST
        config.access_token = 'ACCESS_TOKEN'
        self.api_client = petstore_api.ApiClient(config)
        self.pet_api = petstore_api.PetApi(self.api_client)
        self.setUpModels()

    def setUpModels(self):
        self.category = petstore_api.Category(name="dog")
        self.category.id = id_gen()
        self.category.name = "dog"
        self.tag = petstore_api.Tag()
        self.tag.id = id_gen()
        self.tag.name = "blank"
        self.pet = petstore_api.Pet(name="hello kity", photoUrls=["http://foo.bar.com/1", "http://foo.bar.com/2"])
        self.pet.id = id_gen()
        self.pet.status = "sold"
        self.pet.category = self.category
        self.pet.tags = [self.tag]

    def test_404_error(self):
        self.pet_api.add_pet(self.pet)
        assert self.pet.id is not None
        self.pet_api.delete_pet(pet_id=self.pet.id)

        with self.checkRaiseRegex(ApiException, "Pet not found"):
            self.pet_api.get_pet_by_id(pet_id=self.pet.id)

        try:
            self.pet_api.get_pet_by_id(pet_id=self.pet.id)
        except ApiException as e:
            self.assertEqual(e.status, 404)
            self.assertEqual(e.reason, "Not Found")
            self.checkRegex(e.body, "Pet not found")

    def test_500_error(self):
        self.pet_api.add_pet(self.pet)
        assert self.pet.id is not None

        with self.checkRaiseRegex(ApiException, "Internal Server Error"):
            self.pet_api.upload_file(
                pet_id=self.pet.id,
                additional_metadata="special",
                file=None
            )

        try:
            self.pet_api.upload_file(
                pet_id=self.pet.id,
                additional_metadata="special",
                file=None
            )
        except ApiException as e:
            self.assertEqual(e.status, 500)
            self.assertEqual(e.reason, "Internal Server Error")
            self.checkRegex(e.body, "Error 500 Internal Server Error")

    def checkRaiseRegex(self, expected_exception, expected_regex):
        return self.assertRaisesRegex(expected_exception, expected_regex)

    def checkRegex(self, text, expected_regex):
        return self.assertRegex(text, expected_regex)
