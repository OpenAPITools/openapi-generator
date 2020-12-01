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

from collections import namedtuple
import json
import os
import unittest

import petstore_api
from petstore_api import Configuration
from petstore_api.rest import (
    RESTClientObject,
    RESTResponse
)

import six

from petstore_api.exceptions import (
    ApiException,
    ApiValueError,
    ApiTypeError,
)
from petstore_api.api.pet_api import PetApi
from petstore_api.model import pet
from .util import id_gen

import urllib3

if six.PY3:
    from unittest.mock import patch
else:
    from mock import patch

HOST = 'http://localhost/v2'


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
        self.pet_api = PetApi(self.api_client)
        self.setUpModels()
        self.setUpFiles()

    def setUpModels(self):
        from petstore_api.model import category, tag
        self.category = category.Category()
        self.category.id = id_gen()
        self.category.name = "dog"
        self.tag = tag.Tag()
        self.tag.id = id_gen()
        self.tag.name = "python-pet-tag"
        self.pet = pet.Pet(name="hello kity", photo_urls=["http://foo.bar.com/1", "http://foo.bar.com/2"])
        self.pet.id = id_gen()
        self.pet.status = "sold"
        self.pet.category = self.category
        self.pet.tags = [self.tag]

    def setUpFiles(self):
        self.test_file_dir = os.path.join(os.path.dirname(__file__), "..", "testfiles")
        self.test_file_dir = os.path.realpath(self.test_file_dir)

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

    def test_config(self):
        config = Configuration(host=HOST)
        self.assertIsNotNone(config.get_host_settings())
        self.assertEqual(config.get_basic_auth_token(),
                          urllib3.util.make_headers(basic_auth=":").get('authorization'))
        # No authentication scheme has been configured at this point, so auth_settings()
        # should return an empty list.
        self.assertEqual(len(config.auth_settings()), 0)
        # Configure OAuth2 access token and verify the auth_settings have OAuth2 parameters.
        config.access_token = 'MY-ACCESS_TOKEN'
        self.assertEqual(len(config.auth_settings()), 1)
        self.assertIn("petstore_auth", config.auth_settings().keys())
        config.username = "user"
        config.password = "password"
        self.assertEqual(
            config.get_basic_auth_token(),
            urllib3.util.make_headers(basic_auth="user:password").get('authorization'))
        self.assertEqual(len(config.auth_settings()), 2)
        self.assertIn("petstore_auth", config.auth_settings().keys())
        self.assertIn("http_basic_test", config.auth_settings().keys())
        config.username = None
        config.password = None
        self.assertEqual(len(config.auth_settings()), 1)
        self.assertIn("petstore_auth", config.auth_settings().keys())

    def test_timeout(self):
        mock_pool = MockPoolManager(self)
        self.api_client.rest_client.pool_manager = mock_pool

        mock_pool.expect_request('POST', 'http://localhost/v2/pet',
                                 body=json.dumps(self.api_client.sanitize_for_serialization(self.pet)),
                                 headers={'Content-Type': 'application/json',
                                          'Authorization': 'Bearer ACCESS_TOKEN',
                                          'User-Agent': 'OpenAPI-Generator/1.0.0/python'},
                                 preload_content=True, timeout=TimeoutWithEqual(total=5))
        mock_pool.expect_request('POST', 'http://localhost/v2/pet',
                                 body=json.dumps(self.api_client.sanitize_for_serialization(self.pet)),
                                 headers={'Content-Type': 'application/json',
                                          'Authorization': 'Bearer ACCESS_TOKEN',
                                          'User-Agent': 'OpenAPI-Generator/1.0.0/python'},
                                 preload_content=True, timeout=TimeoutWithEqual(connect=1, read=2))

        self.pet_api.add_pet(self.pet, _request_timeout=5)
        self.pet_api.add_pet(self.pet, _request_timeout=(1, 2))

    def test_separate_default_client_instances(self):
        pet_api = PetApi()
        pet_api2 = PetApi()
        self.assertNotEqual(pet_api.api_client, pet_api2.api_client)

        pet_api.api_client.user_agent = 'api client 3'
        pet_api2.api_client.user_agent = 'api client 4'

        self.assertNotEqual(pet_api.api_client.user_agent, pet_api2.api_client.user_agent)

    def test_separate_default_config_instances(self):
        pet_api = PetApi()
        pet_api2 = PetApi()
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
        self.assertIsInstance(result, pet.Pet)

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

        thread = self.pet_api.get_pet_by_id(self.pet.id, async_req=True,
                                            _return_http_data_only=False)
        data, status, headers = thread.get()

        self.assertIsInstance(data, pet.Pet)
        self.assertEqual(status, 200)

    def test_async_exception(self):
        self.pet_api.add_pet(self.pet)

        thread = self.pet_api.get_pet_by_id(-9999999999999, async_req=True)

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

        fetched = self.pet_api.get_pet_by_id(
            pet_id=self.pet.id,
            _return_http_data_only=False
        )
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
        file_path1 = os.path.join(self.test_file_dir, "1px_pic1.png")
        file_path2 = os.path.join(self.test_file_dir, "1px_pic2.png")
        try:
            file = open(file_path1, "rb")
            additional_metadata = "special"
            self.pet_api.upload_file(
                pet_id=self.pet.id,
                additional_metadata=additional_metadata,
                file=file
            )
        except ApiException as e:
            self.fail("upload_file() raised {0} unexpectedly".format(type(e)))
        finally:
            file.close()

        # upload only one file
        try:
            file = open(file_path1, "rb")
            self.pet_api.upload_file(pet_id=self.pet.id, file=file)
        except ApiException as e:
            self.fail("upload_file() raised {0} unexpectedly".format(type(e)))
        finally:
            file.close()

        # upload multiple files
        HTTPResponse = namedtuple(
            'urllib3_response_HTTPResponse',
            ['status', 'reason', 'data', 'getheaders', 'getheader']
        )
        headers = {}
        def get_headers():
            return headers
        def get_header(name, default=None):
            return headers.get(name, default)
        api_respponse = {
            'code': 200,
            'type': 'blah',
            'message': 'file upload succeeded'
        }
        http_response = HTTPResponse(
            status=200,
            reason='OK',
            data=json.dumps(api_respponse).encode('utf-8'),
            getheaders=get_headers,
            getheader=get_header
        )
        mock_response = RESTResponse(http_response)
        try:
            file1 = open(file_path1, "rb")
            file2 = open(file_path2, "rb")
            with patch.object(RESTClientObject, 'request') as mock_method:
                mock_method.return_value = mock_response
                res = self.pet_api.upload_file(
                    pet_id=684696917, files=[file1, file2])
                mock_method.assert_called_with(
                    'POST',
                    'http://localhost/v2/pet/684696917/uploadImage',
                    _preload_content=True,
                    _request_timeout=None,
                    body=None,
                    headers={
                        'Accept': 'application/json',
                        'Content-Type': 'multipart/form-data',
                        'User-Agent': 'OpenAPI-Generator/1.0.0/python',
                        'Authorization': 'Bearer ACCESS_TOKEN'
                    },
                    post_params=[
                        ('files', ('1px_pic1.png', b'\x89PNG\r\n\x1a\n\x00\x00\x00\rIHDR\x00\x00\x00\x01\x00\x00\x00\x01\x08\x00\x00\x00\x00:~\x9bU\x00\x00\x00\nIDATx\x9cc\xfa\x0f\x00\x01\x05\x01\x02\xcf\xa0.\xcd\x00\x00\x00\x00IEND\xaeB`\x82', 'image/png')),
                        ('files', ('1px_pic2.png', b'\x89PNG\r\n\x1a\n\x00\x00\x00\rIHDR\x00\x00\x00\x01\x00\x00\x00\x01\x08\x00\x00\x00\x00:~\x9bU\x00\x00\x00\nIDATx\x9cc\xfa\x0f\x00\x01\x05\x01\x02\xcf\xa0.\xcd\x00\x00\x00\x00IEND\xaeB`\x82', 'image/png'))
                    ],
                    query_params=[]
                )
        except ApiException as e:
            self.fail("upload_file() raised {0} unexpectedly".format(type(e)))
        finally:
            file1.close()
            file2.close()

        # passing in an array of files to when file only allows one
        # raises an exceptions
        try:
            file = open(file_path1, "rb")
            with self.assertRaises(ApiTypeError) as exc:
                self.pet_api.upload_file(pet_id=self.pet.id, file=[file])
        finally:
            file.close()

        # passing in a single file when an array of file is required
        # raises an exception
        try:
            file = open(file_path1, "rb")
            with self.assertRaises(ApiTypeError) as exc:
                self.pet_api.upload_file(pet_id=self.pet.id, files=file)
        finally:
            file.close()

        # passing in a closed file raises an exception
        with self.assertRaises(ApiValueError) as exc:
            file = open(file_path1, "rb")
            file.close()
            self.pet_api.upload_file(pet_id=self.pet.id, file=file)


    def test_delete_pet(self):
        self.pet_api.add_pet(self.pet)
        self.pet_api.delete_pet(pet_id=self.pet.id, api_key="special-key")

        try:
            self.pet_api.get_pet_by_id(pet_id=self.pet.id)
            raise Exception("expected an error")
        except ApiException as e:
            self.assertEqual(404, e.status)

if __name__ == '__main__':
    unittest.main()
