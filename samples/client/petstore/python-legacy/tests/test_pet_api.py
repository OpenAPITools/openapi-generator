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

try:
    from unittest.mock import patch
except ImportError:
    # Python 2.7
    from mock import patch

import petstore_api
from petstore_api import Configuration
from petstore_api.rest import ApiException

from .util import id_gen

import json

import urllib3

HOST = 'http://petstore.swagger.io/v2'


class TimeoutWithEqual(urllib3.Timeout):
    def __init__(self, *arg, **kwargs):
        super(TimeoutWithEqual, self).__init__(*arg, **kwargs)

    def __eq__(self, other):
        return self._read == other._read and self._connect == other._connect and self.total == other.total


class MockPoolManager(object):
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
        self.category = petstore_api.Category()
        self.category.id = id_gen()
        self.category.name = "dog"
        self.tag = petstore_api.Tag()
        self.tag.id = id_gen()
        self.tag.name = "python-pet-tag"
        self.pet = petstore_api.Pet(name="hello kity", photo_urls=["http://foo.bar.com/1", "http://foo.bar.com/2"])
        self.pet.id = id_gen()
        self.pet.status = "sold"
        self.pet.category = self.category
        self.pet.tags = [self.tag]

    def setUpFiles(self):
        self.test_file_dir = os.path.join(os.path.dirname(__file__), "..", "testfiles")
        self.test_file_dir = os.path.realpath(self.test_file_dir)
        self.foo = os.path.join(self.test_file_dir, "foo.png")

    def test_preload_content_flag(self):
        self.pet_api.add_pet(self.pet)

        resp = self.pet_api.find_pets_by_status(status=[self.pet.status], _preload_content=False)

        # return response should at least have read and close methods.
        self.assertTrue(hasattr(resp, 'read'))
        self.assertTrue(hasattr(resp, 'close'))

        # Also we need to make sure we can release the connection to a pool (if exists) when we are done with it.
        self.assertTrue(hasattr(resp, 'release_conn'))

        # Right now, the client returns urllib3.HTTPResponse. If that changed in future, it is probably a breaking
        # change, however supporting above methods should be enough for most usecases. Remove this test case if
        # we followed the breaking change procedure for python client (e.g. increasing major version).
        self.assertTrue(resp.__class__, 'urllib3.response.HTTPResponse')

        resp.close()
        resp.release_conn()

    def test_timeout(self):
        mock_pool = MockPoolManager(self)
        self.api_client.rest_client.pool_manager = mock_pool

        mock_pool.expect_request('POST', HOST + '/pet',
                                 body=json.dumps(self.api_client.sanitize_for_serialization(self.pet)),
                                 headers={'Content-Type': 'application/json',
                                          'Authorization': 'Bearer ACCESS_TOKEN',
                                          'User-Agent': 'OpenAPI-Generator/1.0.0/python'},
                                 preload_content=True, timeout=TimeoutWithEqual(total=5))
        mock_pool.expect_request('POST', HOST + '/pet',
                                 body=json.dumps(self.api_client.sanitize_for_serialization(self.pet)),
                                 headers={'Content-Type': 'application/json',
                                          'Authorization': 'Bearer ACCESS_TOKEN',
                                          'User-Agent': 'OpenAPI-Generator/1.0.0/python'},
                                 preload_content=True, timeout=TimeoutWithEqual(connect=1, read=2))

        self.pet_api.add_pet(self.pet, _request_timeout=5)
        self.pet_api.add_pet(self.pet, _request_timeout=(1, 2))

    def test_auth_settings(self):
        mock_pool = MockPoolManager(self)
        self.api_client.rest_client.pool_manager = mock_pool

        mock_pool.expect_request('POST', HOST + '/pet',
                                 body=json.dumps(self.api_client.sanitize_for_serialization(self.pet)),
                                 headers={'Content-Type': 'application/json',
                                          'Authorization': 'Bearer ACCESS_TOKEN',
                                          'User-Agent': 'OpenAPI-Generator/1.0.0/python'},
                                 preload_content=True, timeout=None)
        mock_pool.expect_request('POST', HOST + '/pet',
                                 body=json.dumps(self.api_client.sanitize_for_serialization(self.pet)),
                                 headers={'Content-Type': 'application/json',
                                          'Authorization': 'Prefix ANOTHER_TOKEN',
                                          'User-Agent': 'OpenAPI-Generator/1.0.0/python'},
                                 preload_content=True, timeout=None)

        self.pet_api.add_pet(self.pet, _request_auth=None)
        self.pet_api.add_pet(self.pet, _request_auth={
            'in': 'header',
            'key': 'Authorization',
            'value': 'Prefix ANOTHER_TOKEN'
        })

    def test_separate_default_client_instances(self):
        pet_api = petstore_api.PetApi()
        pet_api2 = petstore_api.PetApi()
        self.assertNotEqual(pet_api.api_client, pet_api2.api_client)

        pet_api.api_client.user_agent = 'api client 3'
        pet_api2.api_client.user_agent = 'api client 4'

        self.assertNotEqual(pet_api.api_client.user_agent, pet_api2.api_client.user_agent)

    def test_separate_default_config_instances(self):
        pet_api = petstore_api.PetApi()
        pet_api2 = petstore_api.PetApi()
        self.assertNotEqual(pet_api.api_client.configuration, pet_api2.api_client.configuration)

        pet_api.api_client.configuration.host = 'somehost'
        pet_api2.api_client.configuration.host = 'someotherhost'
        self.assertNotEqual(pet_api.api_client.configuration.host, pet_api2.api_client.configuration.host)

    def test_async_request(self):
        thread = self.pet_api.add_pet(self.pet, async_req=True)
        response = thread.get()
        self.assertIsNone(response)

        thread = self.pet_api.get_pet_by_id(self.pet.id, async_req=True)
        result = thread.get()
        self.assertIsInstance(result, petstore_api.Pet)

    def test_async_with_result(self):
        self.pet_api.add_pet(self.pet, async_req=False)

        thread = self.pet_api.get_pet_by_id(self.pet.id, async_req=True)
        thread2 = self.pet_api.get_pet_by_id(self.pet.id, async_req=True)

        response = thread.get()
        response2 = thread2.get()

        self.assertEqual(response.id, self.pet.id)
        self.assertIsNotNone(response2.id, self.pet.id)

    def test_async_with_http_info(self):
        self.pet_api.add_pet(self.pet)

        thread = self.pet_api.get_pet_by_id_with_http_info(self.pet.id, async_req=True)
        data, status, headers = thread.get()

        self.assertIsInstance(data, petstore_api.Pet)
        self.assertEqual(status, 200)

    def test_async_exception(self):
        self.pet_api.add_pet(self.pet)

        thread = self.pet_api.get_pet_by_id("-9999999999999", async_req=True)

        exception = None
        try:
            thread.get()
        except ApiException as e:
            exception = e

        self.assertIsInstance(exception, ApiException)
        self.assertEqual(exception.status, 404)

    def test_add_pet_and_get_pet_by_id(self):
        self.pet_api.add_pet(self.pet)

        fetched = self.pet_api.get_pet_by_id(pet_id=self.pet.id)
        self.assertIsNotNone(fetched)
        self.assertEqual(self.pet.id, fetched.id)
        self.assertIsNotNone(fetched.category)
        self.assertEqual(self.pet.category.name, fetched.category.name)

    def test_add_pet_and_get_pet_by_id_with_http_info(self):
        self.pet_api.add_pet(self.pet)

        fetched = self.pet_api.get_pet_by_id_with_http_info(pet_id=self.pet.id)
        self.assertIsNotNone(fetched)
        self.assertEqual(self.pet.id, fetched[0].id)
        self.assertIsNotNone(fetched[0].category)
        self.assertEqual(self.pet.category.name, fetched[0].category.name)

    def test_update_pet(self):
        self.pet.name = "hello kity with updated"
        self.pet_api.update_pet(self.pet)

        fetched = self.pet_api.get_pet_by_id(pet_id=self.pet.id)
        self.assertIsNotNone(fetched)
        self.assertEqual(self.pet.id, fetched.id)
        self.assertEqual(self.pet.name, fetched.name)
        self.assertIsNotNone(fetched.category)
        self.assertEqual(fetched.category.name, self.pet.category.name)

    def test_find_pets_by_status(self):
        self.pet_api.add_pet(self.pet)

        self.assertIn(
            self.pet.id,
            list(map(lambda x: getattr(x, 'id'), self.pet_api.find_pets_by_status(status=[self.pet.status])))
        )

    def test_find_pets_by_tags(self):
        self.pet_api.add_pet(self.pet)

        self.assertIn(
            self.pet.id,
            list(map(lambda x: getattr(x, 'id'), self.pet_api.find_pets_by_tags(tags=[self.tag.name])))
        )

    def test_update_pet_with_form(self):
        self.pet_api.add_pet(self.pet)

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
        self.pet_api.add_pet(self.pet)
        self.pet_api.delete_pet(pet_id=self.pet.id, api_key="special-key")

        try:
            self.pet_api.get_pet_by_id(pet_id=self.pet.id)
            raise Exception("expected an error")
        except ApiException as e:
            self.assertEqual(404, e.status)

    @patch.object(petstore_api.ApiClient, 'call_api')
    @patch.object(petstore_api.ApiClient, 'select_header_content_type')
    def test_call_select_header_content_type(self, mock_select_ct, mock_call_api):
        mock_select_ct.return_value = 'application/json'
        self.pet_api.add_pet({'id': 1000, 'name': 'my pet'})
        mock_select_ct.assert_called_once_with(
            ['application/json', 'application/xml'],
            'POST',
            {'id': 1000, 'name': 'my pet'})
        mock_call_api.assert_called_once()
        self.assertEqual(mock_call_api.mock_calls[0][1][4],
            {'Content-Type': 'application/json'})

    @patch.object(petstore_api.ApiClient, 'call_api')
    def test_call_with_forced_content_type(self, mock_call_api):
        mock_call_api.return_value = 'application/xml'
        self.pet_api.add_pet('<mock></mock>', _content_type='application/xml')
        mock_call_api.assert_called_once()
        self.assertEqual(mock_call_api.mock_calls[0][1][4],
            {'Content-Type': 'application/xml'})

if __name__ == '__main__':
    unittest.main()
