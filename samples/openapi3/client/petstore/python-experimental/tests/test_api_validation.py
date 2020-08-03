# coding: utf-8

# flake8: noqa

"""
Run the tests.
$ pip install nose (optional)
$ cd OpenAPIetstore-python
$ nosetests -v
"""

import os
import time
import atexit
import datetime
import functools
import json
import sys
import weakref
import unittest
from dateutil.parser import parse
from collections import namedtuple

import petstore_api
from petstore_api.api import pet_api
from petstore_api.model import array_of_enums
from petstore_api.model import format_test
from petstore_api.model import string_enum
import petstore_api.configuration

HOST = 'http://petstore.swagger.io/v2'
MockResponse = namedtuple('MockResponse', 'data')


class ApiClientTests(unittest.TestCase):
    def setUp(self):
        self.api_client = petstore_api.ApiClient()

    def test_configuration(self):
        config = petstore_api.Configuration()
        config.host = 'http://localhost/'

        config.disabled_client_side_validations = ("multipleOf,maximum,exclusiveMaximum,minimum,exclusiveMinimum,"
            "maxLength,minLength,pattern,maxItems,minItems")
        with self.checkRaiseRegex(ValueError, "Invalid keyword: 'foo'"):
            config.disabled_client_side_validations = 'foo'
        config.disabled_client_side_validations = ""

    def test_servers(self):
        config = petstore_api.Configuration(server_index=1, server_variables={'version': 'v1'})
        api_client = petstore_api.ApiClient(config)
        api = pet_api.PetApi(api_client)

        def request(expected_url, method, url, **kwargs):
            assert expected_url == url
            raise RuntimeError('pass')

        api_client.request = functools.partial(request, 'http://path-server-test.petstore.local/v2/pet')
        try:
            api.add_pet({'name': 'pet', 'photo_urls': []})
        except RuntimeError as e:
            assert "pass" == str(e)

        api_client.request = functools.partial(request, 'https://localhost:8080/v1/pet/123456789')
        try:
            api.delete_pet(123456789)
        except RuntimeError as e:
            assert "pass" == str(e)

    def test_array_of_enums(self):
        data = [
            "placed", None
        ]
        response = MockResponse(data=json.dumps(data))
        deserialized = self.api_client.deserialize(response, (array_of_enums.ArrayOfEnums, ), True)
        assert isinstance(deserialized, array_of_enums.ArrayOfEnums)
        assert array_of_enums.ArrayOfEnums([string_enum.StringEnum(v) for v in data]) == deserialized

    def checkRaiseRegex(self, expected_exception, expected_regex):
        if sys.version_info < (3, 0):
            return self.assertRaisesRegexp(expected_exception, expected_regex)

        return self.assertRaisesRegex(expected_exception, expected_regex)


    def test_multiple_of(self):
        inst = format_test.FormatTest(
            byte='3',
            date=datetime.date(2000, 1, 1),
            password="abcdefghijkl",
            integer=30,
            number=65.0,
            float=62.4
        )
        assert isinstance(inst, format_test.FormatTest)

        with self.checkRaiseRegex(petstore_api.exceptions.ApiValueError, "Invalid value for `integer`, value must be a multiple of `2`"):
          inst = format_test.FormatTest(
              byte='3',
              date=datetime.date(2000, 1, 1),
              password="abcdefghijkl",
              integer=31,  # Value is supposed to be multiple of '2'. An error must be raised
              number=65.0,
              float=62.4
          )

    def test_multiple_of_deserialization(self):
        data = {
            'byte': '3',
            'date': '1970-01-01',
            'password': "abcdefghijkl",
            'integer': 30,
            'number': 65.0,
            'float': 62.4,
        }
        response = MockResponse(data=json.dumps(data))
        deserialized = self.api_client.deserialize(response, (format_test.FormatTest,), True)
        self.assertTrue(isinstance(deserialized, format_test.FormatTest))

        with self.checkRaiseRegex(petstore_api.exceptions.ApiValueError, "Invalid value for `integer`, value must be a multiple of `2`"):
          data = {
              'byte': '3',
              'date': '1970-01-01',
              'password': "abcdefghijkl",
              'integer': 31,  # Value is supposed to be multiple of '2'. An error must be raised
              'number': 65.0,
              'float': 62.4,
          }
          response = MockResponse(data=json.dumps(data))
          deserialized = self.api_client.deserialize(response, (format_test.FormatTest,), True)

        # Disable JSON schema validation. No error should be raised during deserialization.
        config = petstore_api.Configuration()
        config.disabled_client_side_validations = "multipleOf"
        api_client = petstore_api.ApiClient(configuration=config)

        data = {
            'byte': '3',
            'date': '1970-01-01',
            'password': "abcdefghijkl",
            'integer': 31, # Value is supposed to be multiple of '2'
            'number': 65.0,
            'float': 62.4,
        }
        response = MockResponse(data=json.dumps(data))
        deserialized = api_client.deserialize(response, (format_test.FormatTest,), True)
        self.assertTrue(isinstance(deserialized, format_test.FormatTest))

        # Disable JSON schema validation but for a different keyword.
        # An error should be raised during deserialization.
        config = petstore_api.Configuration()
        config.disabled_client_side_validations = "maxItems"
        api_client = petstore_api.ApiClient(configuration=config)

        with self.checkRaiseRegex(petstore_api.exceptions.ApiValueError, "Invalid value for `integer`, value must be a multiple of `2`"):
            data = {
                'byte': '3',
                'date': '1970-01-01',
                'password': "abcdefghijkl",
                'integer': 31, # Value is supposed to be multiple of '2'
                'number': 65.0,
                'float': 62.4,
            }
            response = MockResponse(data=json.dumps(data))
            deserialized = api_client.deserialize(response, (format_test.FormatTest,), True)
