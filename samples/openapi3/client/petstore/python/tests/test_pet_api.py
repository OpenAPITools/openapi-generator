# coding: utf-8

# flake8: noqa

"""
Run the tests.
$ docker pull swaggerapi/petstore
$ docker run -d -e SWAGGER_HOST=http://petstore.swagger.io -e SWAGGER_BASE_PATH=/v2 -p 80:8080 swaggerapi/petstore
$ pip install -U pytest
$ cd petstore_api-python
$ pytest
"""

import os
import unittest

import petstore_api
from petstore_api import Configuration
from petstore_api.rest import ApiException

from .util import id_gen

import json

import urllib3

HOST = 'http://localhost/v2'


class TimeoutWithEqual(urllib3.Timeout):
    def __init__(self, *arg, **kwargs):
        super(TimeoutWithEqual, self).__init__(*arg, **kwargs)

    def __eq__(self, other):
        return self._read == other._read and self._connect == other._connect and self.total == other.total


class MockPoolManager(urllib3.PoolManager):
    def __init__(self, tc):
        self._tc = tc
        self._reqs = []

    def expect_request(self, *args, **kwargs):
        self._reqs.append((args, kwargs))

    def request(self, *args, **kwargs):
        self._tc.assertTrue(len(self._reqs) > 0)
        r = self._reqs.pop(0)
        self._tc.maxDiff = None
        self._tc.assertEqual(r[0], args)
        self._tc.assertEqual(r[1], kwargs)
        return urllib3.HTTPResponse(status=200, body=b'test')


class PetApiTests(unittest.TestCase):

    def setUp(self):
        config = Configuration()
        config.host = HOST
        config.access_token = 'ACCESS_TOKEN'
        self.api_client = petstore_api.ApiClient(config)
        self.pet_api = petstore_api.PetApi(self.api_client)
        self.setUpModels()
        self.setUpFiles()

    def setUpModels(self):
        self.category = petstore_api.Category(name="dog")
        self.category.id = id_gen()
        # self.category.name = "dog"
        self.tag = petstore_api.Tag()
        self.tag.id = id_gen()
        self.tag.name = "python-pet-tag"
        self.pet = petstore_api.Pet(name="hello kity", photoUrls=["http://foo.bar.com/1", "http://foo.bar.com/2"])
        self.pet.id = id_gen()
        self.pet.status = "sold"
        self.pet.category = self.category
        self.pet.tags = [self.tag]

        self.pet2 = petstore_api.Pet(name="superman 2", photoUrls=["http://foo.bar.com/1", "http://foo.bar.com/2"])
        self.pet2.id = id_gen() + 1
        self.pet2.status = "available"
        self.pet2.category = self.category
        self.pet2.tags = [self.tag]

    def setUpFiles(self):
        self.test_file_dir = os.path.join(os.path.dirname(__file__), "..", "testfiles")
        self.test_file_dir = os.path.realpath(self.test_file_dir)
        self.foo = os.path.join(self.test_file_dir, "pix.gif")

    def test_timeout(self):
        mock_pool = MockPoolManager(self)
        self.api_client.rest_client.pool_manager = mock_pool

        mock_pool.expect_request('POST', HOST + '/pet',
                                 body=json.dumps(self.api_client.sanitize_for_serialization(self.pet)),
                                 headers={'Content-Type': 'application/json',
                                          'Authorization': 'Bearer ACCESS_TOKEN',
                                          'User-Agent': 'OpenAPI-Generator/1.0.0/python'},
                                 preload_content=False, timeout=TimeoutWithEqual(total=5.0))
        mock_pool.expect_request('POST', HOST + '/pet',
                                 body=json.dumps(self.api_client.sanitize_for_serialization(self.pet)),
                                 headers={'Content-Type': 'application/json',
                                          'Authorization': 'Bearer ACCESS_TOKEN',
                                          'User-Agent': 'OpenAPI-Generator/1.0.0/python'},
                                 preload_content=False, timeout=TimeoutWithEqual(connect=1.0, read=2.0))

        self.pet_api.add_pet(self.pet, _request_timeout=5.0)
        self.pet_api.add_pet(self.pet, _request_timeout=(1.0, 2.0))

    def test_accept_header_serialization(self):
        (_, _, headers, *_) = self.pet_api._get_pet_by_id_serialize(
            pet_id=self.pet.id,
            _request_auth=None,
            _content_type=None,
            _headers=None,
            _host_index=0
        )
        self.assertEqual(headers['Accept'], 'application/json')

        (_, _, headers_overwritten, *_) = self.pet_api._get_pet_by_id_serialize(
            pet_id=self.pet.id,
            _request_auth=None,
            _content_type=None,
            _headers={'Accept': 'text/plain'},
            _host_index=0
        )
        self.assertEqual(headers_overwritten['Accept'], 'text/plain')

    def test_separate_default_config_instances(self):
        # ensure the default api client is used
        pet_api = petstore_api.PetApi()
        pet_api2 = petstore_api.PetApi()
        self.assertEqual(id(pet_api.api_client), id(pet_api2.api_client))
        # ensure the default configuration is used
        self.assertEqual(id(pet_api.api_client.configuration), id(pet_api2.api_client.configuration))

    def test_add_pet_and_get_pet_by_id(self):
        self.pet_api.add_pet(self.pet)
        assert self.pet.id is not None

        fetched = self.pet_api.get_pet_by_id(pet_id=self.pet.id)
        assert fetched is not None
        self.assertEqual(self.pet.id, fetched.id)
        assert fetched.category is not None
        assert self.pet.category is not None
        self.assertEqual(self.pet.category.name, fetched.category.name)

    def test_add_pet_and_get_pet_by_id_without_preload_content_flag(self):
        self.pet_api.add_pet(self.pet)
        assert self.pet.id is not None

        fetched = self.pet_api.get_pet_by_id_without_preload_content(pet_id=self.pet.id)

        self.assertIsInstance(fetched, urllib3.HTTPResponse)
        self.assertFalse(fetched.closed)
        read = fetched.read()
        self.assertTrue(fetched.closed)
        self.assertIsInstance(read, bytes)
        self.assertEqual(fetched.data, b'')
        self.assertEqual(fetched.read(), b'')
        self.assertEqual(fetched.read(10), b'')
        self.assertTrue(read.decode("utf-8").startswith('{"id":'))

    def test_add_pet_and_get_pet_by_id_with_http_info(self):
        self.pet_api.add_pet(self.pet)
        assert self.pet.id is not None

        # fetched is an ApiResponse object
        fetched = self.pet_api.get_pet_by_id_with_http_info(pet_id=self.pet.id)
        assert fetched.data is not None
        self.assertEqual(self.pet.id, fetched.data.id)
        assert fetched.data.category is not None
        assert self.pet.category is not None
        self.assertEqual(self.pet.category.name, fetched.data.category.name)

    def test_get_pet_by_id_serialize(self):
        self.pet_api.add_pet(self.pet)
        assert self.pet.id is not None

        fetched = self.pet_api._get_pet_by_id_serialize(self.pet.id, None, None, None, None)
        assert fetched is not None
        self.assertIsInstance(fetched, tuple)

    def test_update_pet(self):
        self.pet.name = "hello kity with updated"
        self.pet_api.update_pet(self.pet)
        assert self.pet.id is not None

        fetched = self.pet_api.get_pet_by_id(pet_id=self.pet.id)
        assert fetched is not None
        self.assertEqual(self.pet.id, fetched.id)
        self.assertEqual(self.pet.name, fetched.name)
        assert fetched.category is not None
        assert self.pet.category is not None
        self.assertEqual(fetched.category.name, self.pet.category.name)

    def test_find_pets_by_status(self):
        self.pet_api.add_pet(self.pet)
        self.pet_api.add_pet(self.pet2)

        results = self.pet_api.find_pets_by_status(status=["pending", "available"])
        self.assertIsInstance(results, list)
        self.assertTrue(len(results) > 1)
        # TODO
        #for result in results:
        #    self.assertTrue(result.status in ["sold", "available"])

    def test_find_pets_by_tags(self):
        self.pet_api.add_pet(self.pet)
        assert self.pet.id is not None
        assert self.tag.name is not None

        self.assertIn(
            self.pet.id,
            list(map(lambda x: getattr(x, 'id'), self.pet_api.find_pets_by_tags(tags=[self.tag.name, "here is another tag"])))
        )

    def test_update_pet_with_form(self):
        self.pet_api.add_pet(self.pet)
        assert self.pet.id is not None

        name = "hello kity with form updated abc"
        status = "pending"
        self.pet_api.update_pet_with_form(pet_id=self.pet.id, name=name, status=status)

        fetched = self.pet_api.get_pet_by_id(pet_id=self.pet.id)
        self.assertEqual(self.pet.id, fetched.id)
        self.assertEqual(name, fetched.name)
        self.assertEqual(status, fetched.status)

    def test_upload_file(self):
        assert self.pet.id is not None
        # upload file with form parameter
        try:
            additional_metadata = "special data 123"
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

        try:
            with open(self.foo, 'rb') as binary_file:
                data = binary_file.read()
            self.pet_api.upload_file(pet_id=self.pet.id, file=data)
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


if __name__ == '__main__':
    unittest.main()
