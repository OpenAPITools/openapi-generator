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
        self.setUpModels()

    def setUpModels(self):
        self.category = SwaggerPetstore.Category()
        self.category.id = int(time.time())
        self.category.name = "dog"
        self.tag = SwaggerPetstore.Tag()
        self.tag.id = int(time.time())
        self.tag.name = "blank"
        self.pet = SwaggerPetstore.Pet()
        self.pet.id = int(time.time())
        self.pet.name = "hello kity"
        self.pet.photo_urls = ["http://foo.bar.com/1", "http://foo.bar.com/2"]
        self.pet.status = "sold"
        self.pet.category = self.category
        self.pet.tags = [self.tag]

    def tearDown(self):
        time.sleep(1)

    def test_404_error(self):
        self.pet_api.add_pet(body=self.pet)
        self.pet_api.delete_pet(pet_id=self.pet.id)
        
        with self.assertRaisesRegexp(ApiException, "Pet not found"):
            self.pet_api.get_pet_by_id(pet_id=self.pet.id)

        try:
            self.pet_api.get_pet_by_id(pet_id=self.pet.id)
        except ApiException as e:
            self.assertEqual(e.status, 404)
            self.assertEqual(e.reason, "Not Found")
            self.assertDictEqual(e.body, {'message': 'Pet not found', 'code': 1, 'type': 'error'})

    def test_500_error(self):
        self.pet_api.add_pet(body=self.pet)

        with self.assertRaisesRegexp(ApiException, "Internal Server Error"):
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
            self.assertRegexpMatches(e.body, "Error 500 Internal Server Error")
