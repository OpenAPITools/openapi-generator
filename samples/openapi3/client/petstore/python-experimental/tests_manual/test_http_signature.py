# # coding: utf-8
#
# # flake8: noqa
#
# """
# Run the tests.
# $ docker pull swaggerapi/petstore
# $ docker run -d -e SWAGGER_HOST=http://petstore.swagger.io -e SWAGGER_BASE_PATH=/v2 -p 80:8080 swaggerapi/petstore
# $ pip install nose (optional)
# $ cd petstore_api-python
# $ nosetests -v
# """
#
# from collections import namedtuple
# from datetime import datetime, timedelta
# import base64
# import json
# import os
# import re
# import shutil
# import unittest
# from urllib.parse import urlencode, urlparse
#
# from Crypto.Hash import SHA256, SHA512
# from Crypto.PublicKey import ECC, RSA
# from Crypto.Signature import pkcs1_15, pss, DSS
#
# import petstore_api
# from petstore_api.model import category, tag, pet
# from petstore_api.api.pet_api import PetApi
# from petstore_api import Configuration, signing
# from petstore_api.rest import (
#     RESTClientObject,
#     RESTResponse
# )
#
# from petstore_api.exceptions import (
#     ApiException,
#     ApiValueError,
#     ApiTypeError,
# )
#
# from .util import id_gen
#
# import urllib3
#
# from unittest.mock import patch
#
# HOST = 'http://localhost/v2'
#
# # This test RSA private key below is published in Appendix C 'Test Values' of
# # https://www.ietf.org/id/draft-cavage-http-signatures-12.txt
# RSA_TEST_PRIVATE_KEY = """-----BEGIN RSA PRIVATE KEY-----
# MIICXgIBAAKBgQDCFENGw33yGihy92pDjZQhl0C36rPJj+CvfSC8+q28hxA161QF
# NUd13wuCTUcq0Qd2qsBe/2hFyc2DCJJg0h1L78+6Z4UMR7EOcpfdUE9Hf3m/hs+F
# UR45uBJeDK1HSFHD8bHKD6kv8FPGfJTotc+2xjJwoYi+1hqp1fIekaxsyQIDAQAB
# AoGBAJR8ZkCUvx5kzv+utdl7T5MnordT1TvoXXJGXK7ZZ+UuvMNUCdN2QPc4sBiA
# QWvLw1cSKt5DsKZ8UETpYPy8pPYnnDEz2dDYiaew9+xEpubyeW2oH4Zx71wqBtOK
# kqwrXa/pzdpiucRRjk6vE6YY7EBBs/g7uanVpGibOVAEsqH1AkEA7DkjVH28WDUg
# f1nqvfn2Kj6CT7nIcE3jGJsZZ7zlZmBmHFDONMLUrXR/Zm3pR5m0tCmBqa5RK95u
# 412jt1dPIwJBANJT3v8pnkth48bQo/fKel6uEYyboRtA5/uHuHkZ6FQF7OUkGogc
# mSJluOdc5t6hI1VsLn0QZEjQZMEOWr+wKSMCQQCC4kXJEsHAve77oP6HtG/IiEn7
# kpyUXRNvFsDE0czpJJBvL/aRFUJxuRK91jhjC68sA7NsKMGg5OXb5I5Jj36xAkEA
# gIT7aFOYBFwGgQAQkWNKLvySgKbAZRTeLBacpHMuQdl1DfdntvAyqpAZ0lY0RKmW
# G6aFKaqQfOXKCyWoUiVknQJAXrlgySFci/2ueKlIE1QqIiLSZ8V8OlpFLRnb1pzI
# 7U1yQXnTAEFYM560yJlzUpOb1V4cScGd365tiSMvxLOvTA==
# -----END RSA PRIVATE KEY-----"""
#
#
# class TimeoutWithEqual(urllib3.Timeout):
#     def __init__(self, *arg, **kwargs):
#         super(TimeoutWithEqual, self).__init__(*arg, **kwargs)
#
#     def __eq__(self, other):
#         return self._read == other._read and self._connect == other._connect and self.total == other.total
#
# class MockPoolManager(object):
#     def __init__(self, tc):
#         self._tc = tc
#         self._reqs = []
#
#     def expect_request(self, *args, **kwargs):
#         self._reqs.append((args, kwargs))
#
#     def set_signing_config(self, signing_cfg):
#         self.signing_cfg = signing_cfg
#         self._tc.assertIsNotNone(self.signing_cfg)
#         self.pubkey = self.signing_cfg.get_public_key()
#         self._tc.assertIsNotNone(self.pubkey)
#
#     def request(self, *actual_request_target, **actual_request_headers_and_body):
#         self._tc.assertTrue(len(self._reqs) > 0)
#         expected_results = self._reqs.pop(0)
#         self._tc.maxDiff = None
#         expected_request_target = expected_results[0] # The expected HTTP method and URL path.
#         expected_request_headers_and_body = expected_results[1] # dict that contains the expected body, headers
#         self._tc.assertEqual(expected_request_target, actual_request_target)
#         # actual_request_headers_and_body is a dict that contains the actual body, headers
#         for k, expected in expected_request_headers_and_body.items():
#             self._tc.assertIn(k, actual_request_headers_and_body)
#             if k == 'body':
#                 actual_body = actual_request_headers_and_body[k]
#                 self._tc.assertEqual(expected, actual_body)
#             elif k == 'headers':
#                 actual_headers = actual_request_headers_and_body[k]
#                 for expected_header_name, expected_header_value in expected.items():
#                     # Validate the generated request contains the expected header.
#                     self._tc.assertIn(expected_header_name, actual_headers)
#                     actual_header_value = actual_headers[expected_header_name]
#                     # Compare the actual value of the header against the expected value.
#                     pattern = re.compile(expected_header_value)
#                     m = pattern.match(actual_header_value)
#                     self._tc.assertTrue(m, msg="Expected:\n{0}\nActual:\n{1}".format(
#                                 expected_header_value,actual_header_value))
#                     if expected_header_name == 'Authorization':
#                         self._validate_authorization_header(
#                             expected_request_target, actual_headers, actual_header_value)
#             elif k == 'timeout':
#                 self._tc.assertEqual(expected, actual_request_headers_and_body[k])
#         return urllib3.HTTPResponse(status=200, body=b'test')
#
#     def _validate_authorization_header(self, request_target, actual_headers, authorization_header):
#         """Validate the signature.
#         """
#         # Extract (created)
#         r1 = re.compile(r'created=([0-9]+)')
#         m1 = r1.search(authorization_header)
#         self._tc.assertIsNotNone(m1)
#         created = m1.group(1)
#
#         # Extract list of signed headers
#         r1 = re.compile(r'headers="([^"]+)"')
#         m1 = r1.search(authorization_header)
#         self._tc.assertIsNotNone(m1)
#         headers = m1.group(1).split(' ')
#         signed_headers_list = []
#         for h in headers:
#             if h == '(created)':
#                 signed_headers_list.append((h, created))
#             elif h == '(request-target)':
#                 url = request_target[1]
#                 target_path = urlparse(url).path
#                 signed_headers_list.append((h, "{0} {1}".format(request_target[0].lower(), target_path)))
#             else:
#                 value = next((v for k, v in actual_headers.items() if k.lower() == h), None)
#                 self._tc.assertIsNotNone(value)
#                 signed_headers_list.append((h, value))
#         header_items = [
#             "{0}: {1}".format(key.lower(), value) for key, value in signed_headers_list]
#         string_to_sign = "\n".join(header_items)
#         digest = None
#         if self.signing_cfg.hash_algorithm == signing.HASH_SHA512:
#             digest = SHA512.new()
#         elif self.signing_cfg.hash_algorithm == signing.HASH_SHA256:
#             digest = SHA256.new()
#         else:
#             self._tc.fail("Unsupported hash algorithm: {0}".format(self.signing_cfg.hash_algorithm))
#         digest.update(string_to_sign.encode())
#         b64_body_digest = base64.b64encode(digest.digest()).decode()
#
#         # Extract the signature
#         r2 = re.compile(r'signature="([^"]+)"')
#         m2 = r2.search(authorization_header)
#         self._tc.assertIsNotNone(m2)
#         b64_signature = m2.group(1)
#         signature = base64.b64decode(b64_signature)
#         # Build the message
#         signing_alg = self.signing_cfg.signing_algorithm
#         if signing_alg is None:
#             # Determine default
#             if isinstance(self.pubkey, RSA.RsaKey):
#                 signing_alg = signing.ALGORITHM_RSASSA_PSS
#             elif isinstance(self.pubkey, ECC.EccKey):
#                 signing_alg = signing.ALGORITHM_ECDSA_MODE_FIPS_186_3
#             else:
#                 self._tc.fail("Unsupported key: {0}".format(type(self.pubkey)))
#
#         if signing_alg == signing.ALGORITHM_RSASSA_PKCS1v15:
#             pkcs1_15.new(self.pubkey).verify(digest, signature)
#         elif signing_alg == signing.ALGORITHM_RSASSA_PSS:
#             pss.new(self.pubkey).verify(digest, signature)
#         elif signing_alg == signing.ALGORITHM_ECDSA_MODE_FIPS_186_3:
#             verifier = DSS.new(key=self.pubkey, mode=signing.ALGORITHM_ECDSA_MODE_FIPS_186_3,
#                                 encoding='der')
#             verifier.verify(digest, signature)
#         elif signing_alg == signing.ALGORITHM_ECDSA_MODE_DETERMINISTIC_RFC6979:
#             verifier = DSS.new(key=self.pubkey, mode=signing.ALGORITHM_ECDSA_MODE_DETERMINISTIC_RFC6979,
#                                 encoding='der')
#             verifier.verify(digest, signature)
#         else:
#             self._tc.fail("Unsupported signing algorithm: {0}".format(signing_alg))
#
# class PetApiTests(unittest.TestCase):
#
#     @classmethod
#     def setUpClass(cls):
#         cls.setUpModels()
#         cls.setUpFiles()
#
#     @classmethod
#     def tearDownClass(cls):
#         file_paths = [
#             cls.rsa_key_path,
#             cls.rsa4096_key_path,
#             cls.ec_p521_key_path,
#         ]
#         for file_path in file_paths:
#             os.unlink(file_path)
#
#     @classmethod
#     def setUpModels(cls):
#         cls.category = category.Category()
#         cls.category.id = id_gen()
#         cls.category.name = "dog"
#         cls.tag = tag.Tag()
#         cls.tag.id = id_gen()
#         cls.tag.name = "python-pet-tag"
#         cls.pet = pet.Pet(
#             name="hello kity",
#             photo_urls=["http://foo.bar.com/1", "http://foo.bar.com/2"]
#         )
#         cls.pet.id = id_gen()
#         cls.pet.status = "sold"
#         cls.pet.category = cls.category
#         cls.pet.tags = [cls.tag]
#
#     @classmethod
#     def setUpFiles(cls):
#         cls.test_file_dir = os.path.join(
#             os.path.dirname(__file__), "..", "testfiles")
#         cls.test_file_dir = os.path.realpath(cls.test_file_dir)
#         if not os.path.exists(cls.test_file_dir):
#             os.mkdir(cls.test_file_dir)
#
#         cls.private_key_passphrase = 'test-passphrase'
#         cls.rsa_key_path = os.path.join(cls.test_file_dir, 'rsa.pem')
#         cls.rsa4096_key_path = os.path.join(cls.test_file_dir, 'rsa4096.pem')
#         cls.ec_p521_key_path = os.path.join(cls.test_file_dir, 'ecP521.pem')
#
#         if not os.path.exists(cls.rsa_key_path):
#             with open(cls.rsa_key_path, 'w') as f:
#                 f.write(RSA_TEST_PRIVATE_KEY)
#
#         if not os.path.exists(cls.rsa4096_key_path):
#             key = RSA.generate(4096)
#             private_key = key.export_key(
#                 passphrase=cls.private_key_passphrase,
#                 protection='PEM'
#             )
#             with open(cls.rsa4096_key_path, "wb") as f:
#                 f.write(private_key)
#
#         if not os.path.exists(cls.ec_p521_key_path):
#             key = ECC.generate(curve='P-521')
#             private_key = key.export_key(
#                 format='PEM',
#                 passphrase=cls.private_key_passphrase,
#                 use_pkcs8=True,
#                 protection='PBKDF2WithHMAC-SHA1AndAES128-CBC'
#             )
#             with open(cls.ec_p521_key_path, "wt") as f:
#                 f.write(private_key)
#
#     def test_valid_http_signature(self):
#         privkey_path = self.rsa_key_path
#         signing_cfg = signing.HttpSigningConfiguration(
#             key_id="my-key-id",
#             signing_scheme=signing.SCHEME_HS2019,
#             private_key_path=privkey_path,
#             private_key_passphrase=self.private_key_passphrase,
#             signing_algorithm=signing.ALGORITHM_RSASSA_PKCS1v15,
#             signed_headers=[
#                 signing.HEADER_REQUEST_TARGET,
#                 signing.HEADER_CREATED,
#                 signing.HEADER_HOST,
#                 signing.HEADER_DATE,
#                 signing.HEADER_DIGEST,
#                 'Content-Type'
#             ]
#         )
#         config = Configuration(host=HOST, signing_info=signing_cfg)
#         # Set the OAuth2 acces_token to None. Here we are interested in testing
#         # the HTTP signature scheme.
#         config.access_token = None
#
#         api_client = petstore_api.ApiClient(config)
#         pet_api = PetApi(api_client)
#
#         mock_pool = MockPoolManager(self)
#         api_client.rest_client.pool_manager = mock_pool
#
#         mock_pool.set_signing_config(signing_cfg)
#         mock_pool.expect_request('POST', HOST + '/pet',
#                                  body=json.dumps(api_client.sanitize_for_serialization(self.pet)),
#                                  headers={'Content-Type': r'application/json',
#                                           'Authorization': r'Signature keyId="my-key-id",algorithm="hs2019",created=[0-9]+,'
#                                                 r'headers="\(request-target\) \(created\) host date digest content-type",'
#                                                 r'signature="[a-zA-Z0-9+/=]+"',
#                                           'User-Agent': r'OpenAPI-Generator/1.0.0/python'},
#                                  preload_content=True, timeout=None)
#
#         pet_api.add_pet(self.pet)
#
#     def test_valid_http_signature_with_defaults(self):
#         privkey_path = self.rsa4096_key_path
#         signing_cfg = signing.HttpSigningConfiguration(
#             key_id="my-key-id",
#             signing_scheme=signing.SCHEME_HS2019,
#             private_key_path=privkey_path,
#             private_key_passphrase=self.private_key_passphrase,
#         )
#         config = Configuration(host=HOST, signing_info=signing_cfg)
#         # Set the OAuth2 acces_token to None. Here we are interested in testing
#         # the HTTP signature scheme.
#         config.access_token = None
#
#         api_client = petstore_api.ApiClient(config)
#         pet_api = PetApi(api_client)
#
#         mock_pool = MockPoolManager(self)
#         api_client.rest_client.pool_manager = mock_pool
#
#         mock_pool.set_signing_config(signing_cfg)
#         mock_pool.expect_request('POST', HOST + '/pet',
#                                  body=json.dumps(api_client.sanitize_for_serialization(self.pet)),
#                                  headers={'Content-Type': r'application/json',
#                                           'Authorization': r'Signature keyId="my-key-id",algorithm="hs2019",created=[0-9]+,'
#                                                 r'headers="\(created\)",'
#                                                 r'signature="[a-zA-Z0-9+/=]+"',
#                                           'User-Agent': r'OpenAPI-Generator/1.0.0/python'},
#                                  preload_content=True, timeout=None)
#
#         pet_api.add_pet(self.pet)
#
#     def test_valid_http_signature_rsassa_pkcs1v15(self):
#         privkey_path = self.rsa4096_key_path
#         signing_cfg = signing.HttpSigningConfiguration(
#             key_id="my-key-id",
#             signing_scheme=signing.SCHEME_HS2019,
#             private_key_path=privkey_path,
#             private_key_passphrase=self.private_key_passphrase,
#             signing_algorithm=signing.ALGORITHM_RSASSA_PKCS1v15,
#             signed_headers=[
#                 signing.HEADER_REQUEST_TARGET,
#                 signing.HEADER_CREATED,
#             ]
#         )
#         config = Configuration(host=HOST, signing_info=signing_cfg)
#         # Set the OAuth2 acces_token to None. Here we are interested in testing
#         # the HTTP signature scheme.
#         config.access_token = None
#
#         api_client = petstore_api.ApiClient(config)
#         pet_api = PetApi(api_client)
#
#         mock_pool = MockPoolManager(self)
#         api_client.rest_client.pool_manager = mock_pool
#
#         mock_pool.set_signing_config(signing_cfg)
#         mock_pool.expect_request('POST', HOST + '/pet',
#                                  body=json.dumps(api_client.sanitize_for_serialization(self.pet)),
#                                  headers={'Content-Type': r'application/json',
#                                           'Authorization': r'Signature keyId="my-key-id",algorithm="hs2019",created=[0-9]+,'
#                                                 r'headers="\(request-target\) \(created\)",'
#                                                 r'signature="[a-zA-Z0-9+/=]+"',
#                                           'User-Agent': r'OpenAPI-Generator/1.0.0/python'},
#                                  preload_content=True, timeout=None)
#
#         pet_api.add_pet(self.pet)
#
#     def test_valid_http_signature_rsassa_pss(self):
#         privkey_path = self.rsa4096_key_path
#         signing_cfg = signing.HttpSigningConfiguration(
#             key_id="my-key-id",
#             signing_scheme=signing.SCHEME_HS2019,
#             private_key_path=privkey_path,
#             private_key_passphrase=self.private_key_passphrase,
#             signing_algorithm=signing.ALGORITHM_RSASSA_PSS,
#             signed_headers=[
#                 signing.HEADER_REQUEST_TARGET,
#                 signing.HEADER_CREATED,
#             ]
#         )
#         config = Configuration(host=HOST, signing_info=signing_cfg)
#         # Set the OAuth2 acces_token to None. Here we are interested in testing
#         # the HTTP signature scheme.
#         config.access_token = None
#
#         api_client = petstore_api.ApiClient(config)
#         pet_api = PetApi(api_client)
#
#         mock_pool = MockPoolManager(self)
#         api_client.rest_client.pool_manager = mock_pool
#
#         mock_pool.set_signing_config(signing_cfg)
#         mock_pool.expect_request('POST', HOST + '/pet',
#                                  body=json.dumps(api_client.sanitize_for_serialization(self.pet)),
#                                  headers={'Content-Type': r'application/json',
#                                           'Authorization': r'Signature keyId="my-key-id",algorithm="hs2019",created=[0-9]+,'
#                                                 r'headers="\(request-target\) \(created\)",'
#                                                 r'signature="[a-zA-Z0-9+/=]+"',
#                                           'User-Agent': r'OpenAPI-Generator/1.0.0/python'},
#                                  preload_content=True, timeout=None)
#
#         pet_api.add_pet(self.pet)
#
#     def test_valid_http_signature_ec_p521(self):
#         privkey_path = self.ec_p521_key_path
#         signing_cfg = signing.HttpSigningConfiguration(
#             key_id="my-key-id",
#             signing_scheme=signing.SCHEME_HS2019,
#             private_key_path=privkey_path,
#             private_key_passphrase=self.private_key_passphrase,
#             hash_algorithm=signing.HASH_SHA512,
#             signed_headers=[
#                 signing.HEADER_REQUEST_TARGET,
#                 signing.HEADER_CREATED,
#             ]
#         )
#         config = Configuration(host=HOST, signing_info=signing_cfg)
#         # Set the OAuth2 acces_token to None. Here we are interested in testing
#         # the HTTP signature scheme.
#         config.access_token = None
#
#         api_client = petstore_api.ApiClient(config)
#         pet_api = PetApi(api_client)
#
#         mock_pool = MockPoolManager(self)
#         api_client.rest_client.pool_manager = mock_pool
#
#         mock_pool.set_signing_config(signing_cfg)
#         mock_pool.expect_request('POST', HOST + '/pet',
#                                  body=json.dumps(api_client.sanitize_for_serialization(self.pet)),
#                                  headers={'Content-Type': r'application/json',
#                                           'Authorization': r'Signature keyId="my-key-id",algorithm="hs2019",created=[0-9]+,'
#                                                 r'headers="\(request-target\) \(created\)",'
#                                                 r'signature="[a-zA-Z0-9+/=]+"',
#                                           'User-Agent': r'OpenAPI-Generator/1.0.0/python'},
#                                  preload_content=True, timeout=None)
#
#         pet_api.add_pet(self.pet)
#
#     def test_invalid_configuration(self):
#         # Signing scheme must be valid.
#         with self.assertRaises(Exception) as cm:
#             signing_cfg = signing.HttpSigningConfiguration(
#                 key_id="my-key-id",
#                 signing_scheme='foo',
#                 private_key_path=self.ec_p521_key_path
#             )
#         self.assertTrue(re.match('Unsupported security scheme', str(cm.exception)),
#             'Exception message: {0}'.format(str(cm.exception)))
#
#         # Signing scheme must be specified.
#         with self.assertRaises(Exception) as cm:
#             signing_cfg = signing.HttpSigningConfiguration(
#                 key_id="my-key-id",
#                 private_key_path=self.ec_p521_key_path,
#                 signing_scheme=None
#             )
#         self.assertTrue(re.match('Unsupported security scheme', str(cm.exception)),
#             'Exception message: {0}'.format(str(cm.exception)))
#
#         # Private key passphrase is missing but key is encrypted.
#         with self.assertRaises(Exception) as cm:
#             signing_cfg = signing.HttpSigningConfiguration(
#                 key_id="my-key-id",
#                 signing_scheme=signing.SCHEME_HS2019,
#                 private_key_path=self.ec_p521_key_path,
#             )
#         self.assertTrue(re.match('Not a valid clear PKCS#8 structure', str(cm.exception)),
#             'Exception message: {0}'.format(str(cm.exception)))
#
#         # File containing private key must exist.
#         with self.assertRaises(Exception) as cm:
#             signing_cfg = signing.HttpSigningConfiguration(
#                 key_id="my-key-id",
#                 signing_scheme=signing.SCHEME_HS2019,
#                 private_key_path='foobar',
#             )
#         self.assertTrue(re.match('Private key file does not exist', str(cm.exception)),
#             'Exception message: {0}'.format(str(cm.exception)))
#
#         # The max validity must be a positive value.
#         with self.assertRaises(Exception) as cm:
#             signing_cfg = signing.HttpSigningConfiguration(
#                 key_id="my-key-id",
#                 signing_scheme=signing.SCHEME_HS2019,
#                 private_key_path=self.ec_p521_key_path,
#                 signature_max_validity=timedelta(hours=-1)
#             )
#         self.assertTrue(re.match('The signature max validity must be a positive value',
#                                 str(cm.exception)),
#             'Exception message: {0}'.format(str(cm.exception)))
#
#         # Cannot include the 'Authorization' header.
#         with self.assertRaises(Exception) as cm:
#             signing_cfg = signing.HttpSigningConfiguration(
#                 key_id="my-key-id",
#                 signing_scheme=signing.SCHEME_HS2019,
#                 private_key_path=self.ec_p521_key_path,
#                 signed_headers=['Authorization']
#             )
#         self.assertTrue(re.match("'Authorization' header cannot be included", str(cm.exception)),
#             'Exception message: {0}'.format(str(cm.exception)))
#
#         # Cannot specify duplicate headers.
#         with self.assertRaises(Exception) as cm:
#             signing_cfg = signing.HttpSigningConfiguration(
#                 key_id="my-key-id",
#                 signing_scheme=signing.SCHEME_HS2019,
#                 private_key_path=self.ec_p521_key_path,
#                 signed_headers=['Host', 'Date', 'Host']
#             )
#         self.assertTrue(re.match('Cannot have duplicates in the signed_headers parameter',
#                                 str(cm.exception)),
#             'Exception message: {0}'.format(str(cm.exception)))
#
