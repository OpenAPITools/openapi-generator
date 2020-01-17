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
import shutil

import petstore_api
from petstore_api import Configuration, signing
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

# Test RSA private key as published in Appendix C 'Test Values' of
# https://www.ietf.org/id/draft-cavage-http-signatures-12.txt
RSA_TEST_PRIVATE_KEY = """-----BEGIN RSA PRIVATE KEY-----
MIICXgIBAAKBgQDCFENGw33yGihy92pDjZQhl0C36rPJj+CvfSC8+q28hxA161QF
NUd13wuCTUcq0Qd2qsBe/2hFyc2DCJJg0h1L78+6Z4UMR7EOcpfdUE9Hf3m/hs+F
UR45uBJeDK1HSFHD8bHKD6kv8FPGfJTotc+2xjJwoYi+1hqp1fIekaxsyQIDAQAB
AoGBAJR8ZkCUvx5kzv+utdl7T5MnordT1TvoXXJGXK7ZZ+UuvMNUCdN2QPc4sBiA
QWvLw1cSKt5DsKZ8UETpYPy8pPYnnDEz2dDYiaew9+xEpubyeW2oH4Zx71wqBtOK
kqwrXa/pzdpiucRRjk6vE6YY7EBBs/g7uanVpGibOVAEsqH1AkEA7DkjVH28WDUg
f1nqvfn2Kj6CT7nIcE3jGJsZZ7zlZmBmHFDONMLUrXR/Zm3pR5m0tCmBqa5RK95u
412jt1dPIwJBANJT3v8pnkth48bQo/fKel6uEYyboRtA5/uHuHkZ6FQF7OUkGogc
mSJluOdc5t6hI1VsLn0QZEjQZMEOWr+wKSMCQQCC4kXJEsHAve77oP6HtG/IiEn7
kpyUXRNvFsDE0czpJJBvL/aRFUJxuRK91jhjC68sA7NsKMGg5OXb5I5Jj36xAkEA
gIT7aFOYBFwGgQAQkWNKLvySgKbAZRTeLBacpHMuQdl1DfdntvAyqpAZ0lY0RKmW
G6aFKaqQfOXKCyWoUiVknQJAXrlgySFci/2ueKlIE1QqIiLSZ8V8OlpFLRnb1pzI
7U1yQXnTAEFYM560yJlzUpOb1V4cScGd365tiSMvxLOvTA==
-----END RSA PRIVATE KEY-----"""

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
        self.setUpModels()
        self.setUpFiles()

    def tearDown(self):
        if os.path.exists(self.rsa_key_path):
            os.unlink(self.rsa_key_path)

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
        if not os.path.exists(self.test_file_dir):
            os.mkdir(self.test_file_dir)
        self.rsa_key_path = os.path.join(self.test_file_dir, 'rsa.pem')

    def test_http_signature(self):
        with open(self.rsa_key_path, 'w') as f:
          f.write(RSA_TEST_PRIVATE_KEY)

        signing_cfg = signing.HttpSigningConfiguration(
            key_id="my-key-id",
            private_key_path=self.rsa_key_path,
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
        config = Configuration(host=HOST, signing_info=signing_cfg)

        api_client = petstore_api.ApiClient(config)
        pet_api = petstore_api.PetApi(api_client)

        mock_pool = MockPoolManager(self)
        api_client.rest_client.pool_manager = mock_pool

        mock_pool.expect_request('POST', 'http://localhost/v2/pet',
                                 body=json.dumps(api_client.sanitize_for_serialization(self.pet)),
                                 headers={'Content-Type': 'application/json',
                                          'Authorization': 'Bearer ',
                                          'User-Agent': 'OpenAPI-Generator/1.0.0/python'},
                                 preload_content=True, timeout=TimeoutWithEqual(total=5))
        mock_pool.expect_request('POST', 'http://localhost/v2/pet',
                                 body=json.dumps(api_client.sanitize_for_serialization(self.pet)),
                                 headers={'Content-Type': 'application/json',
                                          'Authorization': 'Bearer ',
                                          'User-Agent': 'OpenAPI-Generator/1.0.0/python'},
                                 preload_content=True, timeout=TimeoutWithEqual(connect=1, read=2))

        pet_api.add_pet(pet, _request_timeout=5)
        pet_api.add_pet(pet, _request_timeout=(1, 2))


