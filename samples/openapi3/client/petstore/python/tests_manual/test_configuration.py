# coding: utf-8

# flake8: noqa

"""
Run the tests.
$ pip install nose (optional)
$ cd OpenAPIetstore-python
$ nosetests -v
"""

import unittest

from unittest.mock import patch
import urllib3
from urllib3._collections import HTTPHeaderDict

import petstore_api
from petstore_api.api_client import ApiClient
from petstore_api.apis.tags import pet_api


class ConfigurationTests(unittest.TestCase):

    def test_configuration(self):
        config = petstore_api.Configuration()
        config.host = 'https://localhost/'

        config.disabled_client_side_validations = ("multipleOf,maximum,exclusiveMaximum,minimum,exclusiveMinimum,"
            "maxLength,minLength,pattern,maxItems,minItems")
        with self.assertRaisesRegex(ValueError, "Invalid keyword: 'foo'"):
            config.disabled_client_side_validations = 'foo'
        config.disabled_client_side_validations = ""

    def test_servers(self):
        config = petstore_api.Configuration(server_index=1, server_variables={'version': 'v1'})
        client = ApiClient(configuration=config)
        api = pet_api.PetApi(client)

        with patch.object(ApiClient, 'request') as mock_request:
            mock_request.return_value = urllib3.HTTPResponse(status=200)
            api.add_pet({'name': 'pet', 'photoUrls': []})
            mock_request.assert_called_with(
                'POST',
                'https://path-server-test.petstore.local/v2/pet',
                headers=HTTPHeaderDict({
                    'Content-Type': 'application/json',
                    'User-Agent': 'OpenAPI-Generator/1.0.0/python'
                }),
                fields=None,
                body=b'{"photoUrls":[],"name":"pet"}',
                stream=False,
                timeout=None,
            )

        with patch.object(ApiClient, 'request') as mock_request:
            mock_request.return_value = urllib3.HTTPResponse(status=200)
            api.delete_pet(path_params=dict(petId=123456789))
            mock_request.assert_called_with(
                'DELETE',
                'https://localhost:8080/v1/pet/123456789',
                headers={'User-Agent': 'OpenAPI-Generator/1.0.0/python'},
                fields=None,
                body=None,
                stream=False,
                timeout=None,
            )