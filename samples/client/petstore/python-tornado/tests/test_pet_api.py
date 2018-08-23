# coding: utf-8

# flake8: noqa

"""
Run the tests.
$ docker pull swaggerapi/petstore
$ docker run -d -e SWAGGER_HOST=http://petstore.swagger.io -e SWAGGER_BASE_PATH=/v2 -p 80:8080 swaggerapi/petstore
$ pip install nose (optional)
$ cd petstore_api-python
$ nosetests -v
"""

import os
import unittest

from tornado.testing import AsyncTestCase, gen_test
from tornado.ioloop import IOLoop

import petstore_api
from petstore_api import Configuration
from petstore_api.rest import ApiException

from .util import id_gen

import json

import urllib3

HOST = 'http://localhost/v2'

class PetApiTests(AsyncTestCase):

    def setUp(self):
        AsyncTestCase.setUp(self)
        config = Configuration()
        config.host = HOST
        self.api_client = petstore_api.ApiClient(config)
        self.pet_api = petstore_api.PetApi(self.api_client)
        self.setUpModels()
        self.setUpFiles()

    def setUpModels(self):
        self.category = petstore_api.Category()
        self.category.id = id_gen()
        self.category.name = "dog"
        self.tag = petstore_api.Tag()
        self.tag.id = id_gen()
        self.tag.name = "opeanpi-generator-python-pet-tag"
        self.pet = petstore_api.Pet(name="hello kity", photo_urls=["http://foo.bar.com/1", "http://foo.bar.com/2"])
        self.pet.id = id_gen()
        self.pet.status = "sold"
        self.pet.category = self.category
        self.pet.tags = [self.tag]

    def setUpFiles(self):
        self.test_file_dir = os.path.join(os.path.dirname(__file__), "..", "testfiles")
        self.test_file_dir = os.path.realpath(self.test_file_dir)
        self.foo = os.path.join(self.test_file_dir, "foo.png")

    def get_new_ioloop(self):
        return IOLoop.instance()

    @gen_test
    def test_separate_default_client_instances(self):
        pet_api = petstore_api.PetApi()
        pet_api2 = petstore_api.PetApi()
        self.assertNotEqual(pet_api.api_client, pet_api2.api_client)

        pet_api.api_client.user_agent = 'api client 3'
        pet_api2.api_client.user_agent = 'api client 4'

        self.assertNotEqual(pet_api.api_client.user_agent, pet_api2.api_client.user_agent)

    @gen_test
    def test_separate_default_config_instances(self):
        pet_api = petstore_api.PetApi()
        pet_api2 = petstore_api.PetApi()
        self.assertNotEqual(pet_api.api_client.configuration, pet_api2.api_client.configuration)

        pet_api.api_client.configuration.host = 'somehost'
        pet_api2.api_client.configuration.host = 'someotherhost'
        self.assertNotEqual(pet_api.api_client.configuration.host, pet_api2.api_client.configuration.host)

    @gen_test
    def test_async_request(self):
        # It works but tornado is async by default and creating threadpool
        # to do it looks crazy ;)
        thread = self.pet_api.add_pet(self.pet, async_req=True)
        response = yield thread.get()
        self.assertIsNone(response)

        thread = self.pet_api.get_pet_by_id(self.pet.id, async_req=True)
        result = yield thread.get()
        self.assertIsInstance(result, petstore_api.Pet)

    @gen_test
    def test_async_with_result(self):
        yield self.pet_api.add_pet(self.pet)

        thread = self.pet_api.get_pet_by_id(self.pet.id, async_req=True)
        thread2 = self.pet_api.get_pet_by_id(self.pet.id, async_req=True)

        response = yield thread.get()
        response2 = yield thread2.get()

        self.assertEquals(response.id, self.pet.id)
        self.assertIsNotNone(response2.id, self.pet.id)

    @gen_test
    def test_tornado_async_with_result(self):
        yield self.pet_api.add_pet(self.pet)

        query1 = self.pet_api.get_pet_by_id(self.pet.id)
        query2 = self.pet_api.get_pet_by_id(self.pet.id)

        response1 = yield query1
        response2 = yield query2

        self.assertEquals(response1.id, self.pet.id)
        self.assertIsNotNone(response2.id, self.pet.id)
 
    @gen_test
    def test_add_pet_and_get_pet_by_id(self):
        yield self.pet_api.add_pet(self.pet)

        fetched = yield self.pet_api.get_pet_by_id(pet_id=self.pet.id)
        self.assertIsNotNone(fetched)
        self.assertEqual(self.pet.id, fetched.id)
        self.assertIsNotNone(fetched.category)
        self.assertEqual(self.pet.category.name, fetched.category.name)

    @gen_test
    def test_add_pet_and_get_pet_by_id_with_http_info(self):
        yield self.pet_api.add_pet(self.pet)

        fetched = yield self.pet_api.get_pet_by_id_with_http_info(pet_id=self.pet.id)
        self.assertIsNotNone(fetched)
        self.assertEqual(self.pet.id, fetched[0].id)
        self.assertIsNotNone(fetched[0].category)
        self.assertEqual(self.pet.category.name, fetched[0].category.name)

    @gen_test
    def test_update_pet(self):
        self.pet.name = "hello kity with updated"
        yield self.pet_api.update_pet(self.pet)

        fetched = yield self.pet_api.get_pet_by_id(pet_id=self.pet.id)
        self.assertIsNotNone(fetched)
        self.assertEqual(self.pet.id, fetched.id)
        self.assertEqual(self.pet.name, fetched.name)
        self.assertIsNotNone(fetched.category)
        self.assertEqual(fetched.category.name, self.pet.category.name)

    @gen_test
    def test_find_pets_by_status(self):
        yield self.pet_api.add_pet(self.pet)
        pets = yield self.pet_api.find_pets_by_status(status=[self.pet.status])
        self.assertIn(
            self.pet.id,
            list(map(lambda x: getattr(x, 'id'), pets))
        )

    @gen_test
    def test_find_pets_by_tags(self):
        yield self.pet_api.add_pet(self.pet)
        pets = yield self.pet_api.find_pets_by_tags(tags=[self.tag.name])
        self.assertIn(
            self.pet.id,
            list(map(lambda x: getattr(x, 'id'), pets))
        )

    @gen_test
    def test_update_pet_with_form(self):
        yield self.pet_api.add_pet(self.pet)

        name = "hello kity with form updated"
        status = "pending"
        yield self.pet_api.update_pet_with_form(pet_id=self.pet.id, name=name, status=status)

        fetched = yield self.pet_api.get_pet_by_id(pet_id=self.pet.id)
        self.assertEqual(self.pet.id, fetched.id)
        self.assertEqual(name, fetched.name)
        self.assertEqual(status, fetched.status)

    @gen_test(timeout=10)
    def test_upload_file(self):
        # upload file with form parameter
        try:
            additional_metadata = "special"
            yield self.pet_api.upload_file(
                pet_id=self.pet.id,
                additional_metadata=additional_metadata,
                file=self.foo
            )
        except ApiException as e:
            self.fail("upload_file() raised {0} unexpectedly".format(type(e)))

        # upload only file
        try:
            yield self.pet_api.upload_file(pet_id=self.pet.id, file=self.foo)
        except ApiException as e:
            self.fail("upload_file() raised {0} unexpectedly".format(type(e)))

    @gen_test
    def test_delete_pet(self):
        yield self.pet_api.add_pet(self.pet)
        yield self.pet_api.delete_pet(pet_id=self.pet.id, api_key="special-key")

        try:
            yield self.pet_api.get_pet_by_id(pet_id=self.pet.id)
            raise Exception("expected an error")
        except ApiException as e:
            self.assertEqual(404, e.status)


if __name__ == '__main__':
    import logging
    logging.basicConfig(level=logging.DEBUG)
    unittest.main()
