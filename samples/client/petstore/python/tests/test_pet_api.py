# coding: utf-8

"""
Run the tests.
$ pip install nose (optional)
$ cd petstore_api-python
$ nosetests -v
"""

import os
import unittest

import petstore_api
from petstore_api.rest import ApiException

from .util import id_gen

HOST = 'http://petstore.swagger.io/v2'

class PetApiTests(unittest.TestCase):

    def setUp(self):
        self.api_client = petstore_api.ApiClient(HOST)
        self.pet_api = petstore_api.PetApi(self.api_client)
        self.setUpModels()
        self.setUpFiles()

    def setUpModels(self):
        self.category = petstore_api.Category()
        self.category.id = id_gen()
        self.category.name = "dog"
        self.tag = petstore_api.Tag()
        self.tag.id = id_gen()
        self.tag.name = "swagger-codegen-python-pet-tag"
        self.pet = petstore_api.Pet()
        self.pet.id = id_gen()
        self.pet.name = "hello kity"
        self.pet.photo_urls = ["http://foo.bar.com/1", "http://foo.bar.com/2"]
        self.pet.status = "sold"
        self.pet.category = self.category
        self.pet.tags = [self.tag]

    def setUpFiles(self):
        self.test_file_dir = os.path.join(os.path.dirname(__file__), "..", "testfiles")
        self.test_file_dir = os.path.realpath(self.test_file_dir)
        self.foo = os.path.join(self.test_file_dir, "foo.png")

    def test_create_api_instance(self):
        pet_api = petstore_api.PetApi()
        pet_api2 = petstore_api.PetApi()
        api_client3 = petstore_api.ApiClient()
        api_client3.user_agent = 'api client 3'
        api_client4 = petstore_api.ApiClient()
        api_client4.user_agent = 'api client 4'
        pet_api3 = petstore_api.PetApi(api_client3)

        # same default api client
        self.assertEqual(pet_api.api_client, pet_api2.api_client)
        # confirm using the default api client in the config module
        self.assertEqual(pet_api.api_client, petstore_api.configuration.api_client)
        # 2 different api clients are not the same
        self.assertNotEqual(api_client3, api_client4)
        # customized pet api not using the default api client
        self.assertNotEqual(pet_api3.api_client, petstore_api.configuration.api_client)
        # customized pet api not using the old pet api's api client
        self.assertNotEqual(pet_api3.api_client, pet_api2.api_client)

    def test_async_request(self):
        self.pet_api.add_pet(body=self.pet)

        def callback_function(data):
            self.assertIsNotNone(data)
            self.assertEqual(data.id, self.pet.id)
            self.assertEqual(data.name, self.pet.name)
            self.assertIsNotNone(data.category)
            self.assertEqual(data.category.id, self.pet.category.id)
            self.assertEqual(data.category.name, self.pet.category.name)
            self.assertTrue(isinstance(data.tags, list))
            self.assertEqual(data.tags[0].id, self.pet.tags[0].id)
            self.assertEqual(data.tags[0].name, self.pet.tags[0].name)

        thread = self.pet_api.get_pet_by_id(pet_id=self.pet.id, callback=callback_function)
        thread.join(10)
        if thread.isAlive():
            self.fail("Request timeout")

    def test_add_pet_and_get_pet_by_id(self):
        self.pet_api.add_pet(body=self.pet)

        fetched = self.pet_api.get_pet_by_id(pet_id=self.pet.id)
        self.assertIsNotNone(fetched)
        self.assertEqual(self.pet.id, fetched.id)
        self.assertIsNotNone(fetched.category)
        self.assertEqual(self.pet.category.name, fetched.category.name)

    def test_async_add_pet_and_get_pet_by_id(self):
        self.pet_api.add_pet(body=self.pet)

        def callback_function(data):
            #fetched = self.pet_api.get_pet_by_id(pet_id=self.pet.id)
            self.assertIsNotNone(data)
            self.assertEqual(self.pet.id, data.id)
            self.assertIsNotNone(data.category)
            self.assertEqual(self.pet.category.name, data.category.name)

        thread = self.pet_api.get_pet_by_id(pet_id=self.pet.id, callback=callback_function)
        thread.join(10)
        if thread.isAlive():
            self.fail("Request timeout")

    def test_add_pet_and_get_pet_by_id_with_http_info(self):
        self.pet_api.add_pet(body=self.pet)

        fetched = self.pet_api.get_pet_by_id_with_http_info(pet_id=self.pet.id)
        self.assertIsNotNone(fetched)
        self.assertEqual(self.pet.id, fetched[0].id)
        self.assertIsNotNone(fetched[0].category)
        self.assertEqual(self.pet.category.name, fetched[0].category.name)

    def test_async_add_pet_and_get_pet_by_id_with_http_info(self):
        self.pet_api.add_pet(body=self.pet)

        def callback_function(data):
            #fetched = self.pet_api.get_pet_by_id_with_http_info(pet_id=self.pet.id)
            self.assertIsNotNone(data)
            self.assertEqual(self.pet.id, data[0].id)
            self.assertIsNotNone(data[0].category)
            self.assertEqual(self.pet.category.name, data[0].category.name)

        thread = self.pet_api.get_pet_by_id_with_http_info(pet_id=self.pet.id, callback=callback_function)
        thread.join(10)
        if thread.isAlive():
            self.fail("Request timeout")

    def test_update_pet(self):
        self.pet.name = "hello kity with updated"
        self.pet_api.update_pet(body=self.pet)

        fetched = self.pet_api.get_pet_by_id(pet_id=self.pet.id)
        self.assertIsNotNone(fetched)
        self.assertEqual(self.pet.id, fetched.id)
        self.assertEqual(self.pet.name, fetched.name)
        self.assertIsNotNone(fetched.category)
        self.assertEqual(fetched.category.name, self.pet.category.name)

    def test_find_pets_by_status(self):
        self.pet_api.add_pet(body=self.pet)

        self.assertIn(
            self.pet.id,
            list(map(lambda x: getattr(x, 'id'), self.pet_api.find_pets_by_status(status=[self.pet.status])))
        )

    @unittest.skip("skipping the test as the method sometimes invalid Petstore object with incorrect status")
    def test_find_pets_by_tags(self):
        self.pet_api.add_pet(body=self.pet)

        self.assertIn(
            self.pet.id,
            list(map(lambda x: getattr(x, 'id'), self.pet_api.find_pets_by_tags(tags=[self.tag.name])))
        )

    def test_update_pet_with_form(self):
        self.pet_api.add_pet(body=self.pet)

        name = "hello kity with form updated"
        status = "pending"
        self.pet_api.update_pet_with_form(pet_id=self.pet.id, name=name, status=status)

        fetched = self.pet_api.get_pet_by_id(pet_id=self.pet.id)
        self.assertEqual(self.pet.id, fetched.id)
        self.assertEqual(name, fetched.name)
        self.assertEqual(status, fetched.status)

    def test_upload_file(self):
        # upload file with form parameter
        try:
            additional_metadata = "special"
            self.pet_api.upload_file(
                pet_id=self.pet.id,
                additional_metadata=additional_metadata,
                file=self.foo
            )
        except ApiException as e:
            self.fail("upload_file() raised {0} unexpectedly".format(type(e)))

        # upload only file
        try:
            self.pet_api.upload_file(pet_id=self.pet.id, file=self.foo)
        except ApiException as e:
            self.fail("upload_file() raised {0} unexpectedly".format(type(e)))

    def test_delete_pet(self):
        self.pet_api.add_pet(body=self.pet)
        self.pet_api.delete_pet(pet_id=self.pet.id, api_key="special-key")

        try:
            self.pet_api.get_pet_by_id(pet_id=self.pet.id)
            raise Exception("expected an error")
        except ApiException as e:
            self.assertEqual(404, e.status)

if __name__ == '__main__':
    unittest.main()
