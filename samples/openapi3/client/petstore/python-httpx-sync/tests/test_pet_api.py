# coding: utf-8

# flake8: noqa

"""
Run the tests.
$ docker pull swaggerapi/petstore
$ docker run -d -e SWAGGER_HOST=http://petstore.swagger.io -e SWAGGER_BASE_PATH=/v2 -p 80:8080 swaggerapi/petstore
$ pytest -vv
"""

import os
import unittest
import httpx

import petstore_api
from petstore_api import Configuration
from petstore_api.rest import ApiException

from .util import id_gen


HOST = "http://localhost:80/v2"


class TestPetApiTests(unittest.TestCase):
    def setUp(self):
        config = Configuration()
        config.host = HOST

        self.api_client = petstore_api.ApiClient(config)
        self.pet_api = petstore_api.PetApi(self.api_client)
        self.setUpModels()
        self.setUpFiles()

    def tearDown(self):
        self.api_client.close()

    def setUpModels(self):
        self.category = petstore_api.Category(id=id_gen(), name="dog")
        # self.category.id = id_gen()
        # self.category.name = "dog"
        self.tag = petstore_api.Tag()
        self.tag.id = id_gen()
        self.tag.name = "openapi-generator-python-pet-tag"
        self.pet = petstore_api.Pet(
            name="hello kity",
            photoUrls=["http://foo.bar.com/1", "http://foo.bar.com/2"],
        )
        self.pet.id = id_gen()
        self.pet.status = "sold"
        self.pet.category = self.category
        self.pet.tags = [self.tag]

    def setUpFiles(self):
        self.test_file_dir = os.path.join(os.path.dirname(__file__), "..", "testfiles")
        self.test_file_dir = os.path.realpath(self.test_file_dir)
        self.foo = os.path.join(self.test_file_dir, "foo.png")

    def test_accept_header_serialization(self):
        (_, _, headers, *_) = self.pet_api._get_pet_by_id_serialize(
            pet_id=self.pet.id,
            _request_auth=None,
            _content_type=None,
            _headers=None,
            _host_index=0,
        )
        self.assertEqual(headers["Accept"], "application/json")

        (_, _, headers_overwritten, *_) = self.pet_api._get_pet_by_id_serialize(
            pet_id=self.pet.id,
            _request_auth=None,
            _content_type=None,
            _headers={"Accept": "text/plain"},
            _host_index=0,
        )
        self.assertEqual(headers_overwritten["Accept"], "text/plain")

    def test_separate_default_client_instances(self):
        pet_api = petstore_api.PetApi()
        pet_api2 = petstore_api.PetApi()
        self.assertEqual(id(pet_api.api_client), id(pet_api2.api_client))

    def test_separate_default_config_instances(self):
        pet_api = petstore_api.PetApi()
        pet_api2 = petstore_api.PetApi()
        self.assertEqual(
            id(pet_api.api_client.configuration), id(pet_api2.api_client.configuration)
        )

    def test_with_result(self):
        self.pet_api.add_pet(self.pet)
        assert self.pet.id is not None

        responses = [
            self.pet_api.get_pet_by_id(self.pet.id),
            self.pet_api.get_pet_by_id(self.pet.id),
        ]

        for response in responses:
            self.assertEqual(response.id, self.pet.id)
        self.assertEqual(len(responses), 2)

    def test_exception(self):
        self.pet_api.add_pet(self.pet)

        try:
            self.pet_api.get_pet_by_id(9999999999999)
        except ApiException as e:
            exception = e

        self.assertIsInstance(exception, ApiException)
        self.assertEqual(exception.status, 404)

    def test_add_pet_and_get_pet_by_id(self):
        self.pet_api.add_pet(self.pet)
        assert self.pet.id is not None

        fetched = self.pet_api.get_pet_by_id(pet_id=self.pet.id)
        self.assertEqual(self.pet.id, fetched.id)
        assert self.pet.category is not None
        assert fetched.category is not None
        self.assertEqual(self.pet.category.name, fetched.category.name)

    def test_add_pet_and_get_pet_by_id_with_http_info(self):
        self.pet_api.add_pet(self.pet)
        assert self.pet.id is not None

        fetched = self.pet_api.get_pet_by_id_with_http_info(pet_id=self.pet.id)
        self.assertEqual(self.pet.id, fetched.data.id)
        assert self.pet.category is not None
        assert fetched.data.category is not None
        self.assertEqual(self.pet.category.name, fetched.data.category.name)

    @unittest.skip("TODO: httpx: httpx.Client.request always preloads content")
    def test_add_pet_and_get_pet_by_id_without_preload_content(self):
        self.pet_api.add_pet(self.pet)
        assert self.pet.id is not None

        fetched = self.pet_api.get_pet_by_id_without_preload_content(
            pet_id=self.pet.id
        )
        self.assertIsInstance(fetched, httpx.Response)
        read = fetched.read()
        self.assertTrue(fetched.is_closed)
        self.assertIsInstance(read, bytes)
        self.assertEqual(fetched.read(), b"")
        self.assertTrue(read.decode("utf-8").startswith('{"id":'))

    def test_update_pet(self):
        self.pet.name = "hello kity with updated"
        self.pet_api.update_pet(self.pet)
        assert self.pet.id is not None

        fetched = self.pet_api.get_pet_by_id(pet_id=self.pet.id)
        self.assertEqual(self.pet.id, fetched.id)
        self.assertEqual(self.pet.name, fetched.name)
        assert self.pet.category is not None
        assert fetched.category is not None
        self.assertEqual(fetched.category.name, self.pet.category.name)

    def test_find_pets_by_status(self):
        self.pet_api.add_pet(self.pet)
        assert self.pet.status is not None
        pets = self.pet_api.find_pets_by_status(status=[self.pet.status])
        self.assertIn(self.pet.id, list(map(lambda x: getattr(x, "id"), pets)))

    def test_find_pets_by_tags(self):
        self.pet_api.add_pet(self.pet)
        assert self.tag.name is not None
        pets = self.pet_api.find_pets_by_tags(tags=[self.tag.name])
        self.assertIn(self.pet.id, list(map(lambda x: getattr(x, "id"), pets)))

    def test_update_pet_with_form(self):
        self.pet_api.add_pet(self.pet)
        assert self.pet.id is not None

        name = "hello kity with form updated"
        status = "pending"
        self.pet_api.update_pet_with_form(
            pet_id=self.pet.id, name=name, status=status
        )

        fetched = self.pet_api.get_pet_by_id(pet_id=self.pet.id)
        self.assertEqual(self.pet.id, fetched.id)
        self.assertEqual(name, fetched.name)
        self.assertEqual(status, fetched.status)

    def test_upload_file(self):
        # upload file with form parameter
        try:
            additional_metadata = "special"
            assert self.pet.id is not None
            self.pet_api.upload_file(
                pet_id=self.pet.id,
                additional_metadata=additional_metadata,
                file=self.foo,
            )
        except ApiException as e:
            self.fail("upload_file() raised {0} unexpectedly".format(type(e)))

        # upload only file
        try:
            self.pet_api.upload_file(pet_id=self.pet.id, file=self.foo)
        except ApiException as e:
            self.fail("upload_file() raised {0} unexpectedly".format(type(e)))

    def test_delete_pet(self):
        self.pet_api.add_pet(self.pet)
        assert self.pet.id is not None
        self.pet_api.delete_pet(pet_id=self.pet.id, api_key="special-key")

        try:
            self.pet_api.get_pet_by_id(pet_id=self.pet.id)
            raise Exception("expected an error")
        except ApiException as e:
            self.assertEqual(404, e.status)

    def test_proxy(self):
        config = Configuration()
        # set not-existent proxy and catch an error to verify that
        # the client library (httpx) tried to use it.
        config.proxy = "http://localhost:8080/proxy"
        with petstore_api.ApiClient(config) as client:
            pet_api = petstore_api.PetApi(client)
            assert self.pet.id is not None

            with self.assertRaises(
                petstore_api.rest.httpx.ConnectError,
            ):
                pet_api.get_pet_by_id(self.pet.id)


if __name__ == "__main__":
    import logging

    logging.basicConfig(level=logging.DEBUG)
    unittest.main()
