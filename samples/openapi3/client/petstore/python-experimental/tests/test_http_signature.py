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
from datetime import datetime, timedelta
import json
import os
import re
import shutil
import unittest
from Crypto.PublicKey import RSA
from Crypto.PublicKey import ECC

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
        # r[0] is the expected HTTP method, URL.
        # args is the actual HTTP method, URL.
        self._tc.assertEqual(r[0], args)
        # r[1] is a dict that contains the expected body, headers
        # kwargs is a dict that contains the actual body, headers
        for k, expected in r[1].items():
            self._tc.assertIn(k, kwargs)
            actual = kwargs[k]
            if k == 'body':
                self._tc.assertEqual(expected, actual)
            elif k == 'headers':
                for expected_header_name, expected_header_value in expected.items():
                    self._tc.assertIn(expected_header_name, actual)
                    actual_header_value = actual[expected_header_name]
                    pattern = re.compile(expected_header_value)
                    m = pattern.match(actual_header_value)
                    self._tc.assertTrue(m, msg="Expected:\n{0}\nActual:\n{1}".format(
                                        expected_header_value,actual_header_value))
            elif k == 'timeout':
                self._tc.assertEqual(expected, actual)
        return urllib3.HTTPResponse(status=200, body=b'test')


class PetApiTests(unittest.TestCase):

    def setUp(self):
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
        if not os.path.exists(self.test_file_dir):
            os.mkdir(self.test_file_dir)

        self.rsa_key_path = os.path.join(self.test_file_dir, 'rsa.pem')
        self.rsa4096_key_path = os.path.join(self.test_file_dir, 'rsa4096.pem')
        self.ec_p521_key_path = os.path.join(self.test_file_dir, 'ecP521.pem')

        if not os.path.exists(self.rsa_key_path):
            with open(self.rsa_key_path, 'w') as f:
                f.write(RSA_TEST_PRIVATE_KEY)

        if not os.path.exists(self.rsa4096_key_path):
            key = RSA.generate(4096)
            private_key = key.export_key()
            with open(self.rsa4096_key_path, "wb") as f:
                f.write(private_key)

        if not os.path.exists(self.ec_p521_key_path):
            key = ECC.generate(curve='P-521')
            private_key = key.export_key(format='PEM')
            with open(self.ec_p521_key_path, "wt") as f:
                f.write(private_key)

    def test_valid_http_signature(self):
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
        # Set the OAuth2 acces_token to None. Here we are interested in testing
        # the HTTP signature scheme.
        config.access_token = None

        api_client = petstore_api.ApiClient(config)
        pet_api = petstore_api.PetApi(api_client)

        mock_pool = MockPoolManager(self)
        api_client.rest_client.pool_manager = mock_pool

        mock_pool.expect_request('POST', 'http://petstore.swagger.io/v2/pet',
                                 body=json.dumps(api_client.sanitize_for_serialization(self.pet)),
                                 headers={'Content-Type': 'application/json',
                                          'Authorization': 'Signature keyId="my-key-id",algorithm="hs2019",created=[0-9]+,'
                                                'headers="\(request-target\) \(created\) host date digest content-type",'
                                                'signature="[a-zA-Z0-9+/]+="',
                                          'User-Agent': 'OpenAPI-Generator/1.0.0/python'},
                                 preload_content=True, timeout=None)

        pet_api.add_pet(self.pet)

    def test_valid_http_signature_with_defaults(self):
        signing_cfg = signing.HttpSigningConfiguration(
            key_id="my-key-id",
            private_key_path=self.rsa4096_key_path,
            signing_scheme=signing.SCHEME_HS2019,
        )
        config = Configuration(host=HOST, signing_info=signing_cfg)
        # Set the OAuth2 acces_token to None. Here we are interested in testing
        # the HTTP signature scheme.
        config.access_token = None

        api_client = petstore_api.ApiClient(config)
        pet_api = petstore_api.PetApi(api_client)

        mock_pool = MockPoolManager(self)
        api_client.rest_client.pool_manager = mock_pool

        mock_pool.expect_request('POST', 'http://petstore.swagger.io/v2/pet',
                                 body=json.dumps(api_client.sanitize_for_serialization(self.pet)),
                                 headers={'Content-Type': 'application/json',
                                          'Authorization': 'Signature keyId="my-key-id",algorithm="hs2019",created=[0-9]+,'
                                                'headers="\(created\)",'
                                                'signature="[a-zA-Z0-9+/]+="',
                                          'User-Agent': 'OpenAPI-Generator/1.0.0/python'},
                                 preload_content=True, timeout=None)

        pet_api.add_pet(self.pet)

    def test_valid_http_signature_rsassa_pkcs1v15(self):
        signing_cfg = signing.HttpSigningConfiguration(
            key_id="my-key-id",
            private_key_path=self.rsa4096_key_path,
            signing_scheme=signing.SCHEME_HS2019,
            signing_algorithm=signing.ALGORITHM_RSASSA_PKCS1v15,
            signed_headers=[
                signing.HEADER_REQUEST_TARGET,
                signing.HEADER_CREATED,
            ]
        )
        config = Configuration(host=HOST, signing_info=signing_cfg)
        # Set the OAuth2 acces_token to None. Here we are interested in testing
        # the HTTP signature scheme.
        config.access_token = None

        api_client = petstore_api.ApiClient(config)
        pet_api = petstore_api.PetApi(api_client)

        mock_pool = MockPoolManager(self)
        api_client.rest_client.pool_manager = mock_pool

        mock_pool.expect_request('POST', 'http://petstore.swagger.io/v2/pet',
                                 body=json.dumps(api_client.sanitize_for_serialization(self.pet)),
                                 headers={'Content-Type': 'application/json',
                                          'Authorization': 'Signature keyId="my-key-id",algorithm="hs2019",created=[0-9]+,'
                                                'headers="\(request-target\) \(created\)",'
                                                'signature="[a-zA-Z0-9+/]+="',
                                          'User-Agent': 'OpenAPI-Generator/1.0.0/python'},
                                 preload_content=True, timeout=None)

        pet_api.add_pet(self.pet)

    def test_valid_http_signature_rsassa_pss(self):
        signing_cfg = signing.HttpSigningConfiguration(
            key_id="my-key-id",
            private_key_path=self.rsa4096_key_path,
            signing_scheme=signing.SCHEME_HS2019,
            signing_algorithm=signing.ALGORITHM_RSASSA_PSS,
            signed_headers=[
                signing.HEADER_REQUEST_TARGET,
                signing.HEADER_CREATED,
            ]
        )
        config = Configuration(host=HOST, signing_info=signing_cfg)
        # Set the OAuth2 acces_token to None. Here we are interested in testing
        # the HTTP signature scheme.
        config.access_token = None

        api_client = petstore_api.ApiClient(config)
        pet_api = petstore_api.PetApi(api_client)

        mock_pool = MockPoolManager(self)
        api_client.rest_client.pool_manager = mock_pool

        mock_pool.expect_request('POST', 'http://petstore.swagger.io/v2/pet',
                                 body=json.dumps(api_client.sanitize_for_serialization(self.pet)),
                                 headers={'Content-Type': 'application/json',
                                          'Authorization': 'Signature keyId="my-key-id",algorithm="hs2019",created=[0-9]+,'
                                                'headers="\(request-target\) \(created\)",'
                                                'signature="[a-zA-Z0-9+/]+="',
                                          'User-Agent': 'OpenAPI-Generator/1.0.0/python'},
                                 preload_content=True, timeout=None)

        pet_api.add_pet(self.pet)

    def test_valid_http_signature_ec_p521(self):
        signing_cfg = signing.HttpSigningConfiguration(
            key_id="my-key-id",
            private_key_path=self.ec_p521_key_path,
            signing_scheme=signing.SCHEME_HS2019,
            signed_headers=[
                signing.HEADER_REQUEST_TARGET,
                signing.HEADER_CREATED,
            ]
        )
        config = Configuration(host=HOST, signing_info=signing_cfg)
        # Set the OAuth2 acces_token to None. Here we are interested in testing
        # the HTTP signature scheme.
        config.access_token = None

        api_client = petstore_api.ApiClient(config)
        pet_api = petstore_api.PetApi(api_client)

        mock_pool = MockPoolManager(self)
        api_client.rest_client.pool_manager = mock_pool

        mock_pool.expect_request('POST', 'http://petstore.swagger.io/v2/pet',
                                 body=json.dumps(api_client.sanitize_for_serialization(self.pet)),
                                 headers={'Content-Type': 'application/json',
                                          'Authorization': 'Signature keyId="my-key-id",algorithm="hs2019",created=[0-9]+,'
                                                'headers="\(request-target\) \(created\)",'
                                                'signature="[a-zA-Z0-9+/]+"',
                                          'User-Agent': 'OpenAPI-Generator/1.0.0/python'},
                                 preload_content=True, timeout=None)

        pet_api.add_pet(self.pet)

    def test_invalid_configuration(self):
        # Signing scheme must be valid.
        with self.assertRaises(Exception):
            signing_cfg = signing.HttpSigningConfiguration(
                key_id="my-key-id",
                private_key_path=self.ec_p521_key_path,
                signing_scheme='foo'
            )

        # Signing scheme must be specified.
        with self.assertRaises(Exception):
            signing_cfg = signing.HttpSigningConfiguration(
                key_id="my-key-id",
                private_key_path=self.ec_p521_key_path
            )

        # File containing private key must exist.
        with self.assertRaises(Exception):
            signing_cfg = signing.HttpSigningConfiguration(
                key_id="my-key-id",
                private_key_path='foobar',
                signing_scheme=signing.SCHEME_HS2019
            )

        # The max validity must be a positive value.
        with self.assertRaises(Exception):
            signing_cfg = signing.HttpSigningConfiguration(
                key_id="my-key-id",
                private_key_path=self.ec_p521_key_path,
                signing_scheme=signing.SCHEME_HS2019,
                signature_max_validity=timedelta(hours=-1)
            )

        # Cannot include the 'Authorization' header.
        with self.assertRaises(Exception):
            signing_cfg = signing.HttpSigningConfiguration(
                key_id="my-key-id",
                private_key_path=self.ec_p521_key_path,
                signing_scheme=signing.SCHEME_HS2019,
                signed_headers=['Authorization']
            )

        # Cannot specify duplicate headers.
        with self.assertRaises(Exception):
            signing_cfg = signing.HttpSigningConfiguration(
                key_id="my-key-id",
                private_key_path=self.ec_p521_key_path,
                signing_scheme=signing.SCHEME_HS2019,
                signed_headers=['Host', 'Date', 'Host']
            )

