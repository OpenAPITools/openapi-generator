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

    def test_http_signature(self):
        config = HttpSigningConfiguration(
            key_id="my-key-id",
            private_key_path="rsa.pem",
            signing_scheme=signing.SCHEME_HS2019,
            signing_algorithm=signing.ALGORITHM_RSASSA_PKCS1v15,
            signed_headers=[
                signing.HEADER_REQUEST_TARGET,
                signing.HEADER_CREATED,
                signing.HEADER_HOST,
                signing.HEADER_DATE,
                signing.HEADER_DIGEST,
                'Content-Type'
            ]
        )
        config.host = HOST
        self.api_client = petstore_api.ApiClient(config)
        self.pet_api = petstore_api.PetApi(self.api_client)

        mock_pool = MockPoolManager(self)
        self.api_client.rest_client.pool_manager = mock_pool

        mock_pool.expect_request('POST', 'http://localhost/v2/pet',
                                 body=json.dumps(self.api_client.sanitize_for_serialization(self.pet)),
                                 headers={'Content-Type': 'application/json',
                                          'Authorization': 'Bearer ',
                                          'User-Agent': 'OpenAPI-Generator/1.0.0/python'},
                                 preload_content=True, timeout=TimeoutWithEqual(total=5))
        mock_pool.expect_request('POST', 'http://localhost/v2/pet',
                                 body=json.dumps(self.api_client.sanitize_for_serialization(self.pet)),
                                 headers={'Content-Type': 'application/json',
                                          'Authorization': 'Bearer ',
                                          'User-Agent': 'OpenAPI-Generator/1.0.0/python'},
                                 preload_content=True, timeout=TimeoutWithEqual(connect=1, read=2))

        self.pet_api.add_pet(self.pet, _request_timeout=5)
        self.pet_api.add_pet(self.pet, _request_timeout=(1, 2))


