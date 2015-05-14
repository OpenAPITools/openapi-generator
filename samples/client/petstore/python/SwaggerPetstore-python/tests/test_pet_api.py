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
from SwaggerPetstore.rest import ErrorResponse

HOST = 'http://petstore.swagger.io/v2'


class PetApiTests(unittest.TestCase):

    def setUp(self):
        self.api_client = SwaggerPetstore.ApiClient(HOST)
        self.pet_api = SwaggerPetstore.PetApi(self.api_client)
        self.setUpModels()
        self.setUpFiles()

    def tearDown(self):
        # sleep 1 sec between two every 2 tests
        time.sleep(1)

    def setUpModels(self):
        self.category = SwaggerPetstore.Category()
        self.category.id = 1010
        self.category.name = "dog"
        self.tag = SwaggerPetstore.Tag()
        self.tag.id = 1010
        self.tag.name = "blank"
        self.pet = SwaggerPetstore.Pet()
        self.pet.id = 1010
        self.pet.name = "hello kity"
        self.pet.photo_urls = ["sample urls"]
        self.pet.status = "sold"
        self.pet.category = self.category
        self.pet.tags = [self.tag]

    def setUpFiles(self):
        self.test_file_dir = os.path.join(os.path.dirname(__file__), "..", "testfiles")
        self.test_file_dir = os.path.realpath(self.test_file_dir)
        self.foo = os.path.join(self.test_file_dir, "foo.png")

    def test_1_add_pet(self):
        try:
            self.pet_api.add_pet(body=self.pet)
        except ErrorResponse as e:
            self.fail("add_pet() raised {0} unexpectedly".format(type(e)))

    def test_2_get_pet_by_id(self):
        self.assertEqual(
            dir(self.pet_api.get_pet_by_id(pet_id=self.pet.id)),
            dir(self.pet)
        )

    def test_3_update_pet(self):
        try:
            self.pet.name = "hello kity with updated"
            self.pet_api.update_pet(body=self.pet)
        except ErrorResponse as e:
            self.fail("update_pet() raised {0} unexpectedly".format(type(e)))

    def test_4_find_pets_by_status(self):
        self.assertIn(
            dir(self.pet),
            list(map(dir, self.pet_api.find_pets_by_status(status=["sold"])))
        )

    def test_5_find_pets_by_tags(self):
        self.assertIn(
            dir(self.pet),
            list(map(dir, self.pet_api.find_pets_by_tags(tags=["blank"])))
        )

    def test_6_update_pet_with_form(self):
        try:
            name = "hello kity with form updated"
            status = "pending"
            self.pet_api.update_pet_with_form(
                pet_id=self.pet.id, name=name, status=status
            )
        except ErrorResponse as e:
            self.fail("update_pet_with_form() raised {0} unexpectedly".format(type(e)))

    def test_7_upload_file(self):
        try:
            additional_metadata = "special"
            self.pet_api.upload_file(
                pet_id=self.pet.id,
                additional_metadata=additional_metadata,
                file=self.foo
            )
        except ErrorResponse as e:
            self.fail("upload_file() raised {0} unexpectedly".format(type(e)))

    def test_8_delete_pet(self):
        try:
            api_key = "special-key"
            self.pet_api.delete_pet(pet_id=self.pet.id, api_key=api_key)
        except ErrorResponse as e:
            self.fail("delete_pet() raised {0} unexpectedly".format(type(e)))


if __name__ == '__main__':
    unittest.main()
