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
import re
import shutil
import unittest
from six.moves.urllib.parse import urlencode, urlparse

import petstore_api
from petstore_api import Configuration, signing
from petstore_api.rest import (
    RESTClientObject,
    RESTResponse
)

from petstore_api.model_utils import (
    file_type,
    int,
    model_to_dict,
    str,
)

MockResponse = namedtuple('MockResponse', 'data')

class DiscardUnknownPropertiesTests(unittest.TestCase):

    def test_deserialize_dog_do_not_discard_unknown_properties(self):
        """ deserialize str, Dog) with unknown properties, strict validation is enabled """
        config = Configuration(discard_unknown_keys=False)
        api_client = petstore_api.ApiClient(config)
        data = {
            'dog': {
                "class_name": "Dog",
                "color": "black",
                "breed": "husky",
                "unknown_property": "a-value"
            }
        }
        response = MockResponse(data=json.dumps(data))

        # Deserializing with strict validation raises an exception because the 'unknown_property'
        # is undeclared.
        with self.assertRaises(Exception) as cm:
            deserialized = api_client.deserialize(response,
                ({str: (petstore_api.Dog,)},), True)
        self.assertTrue(re.match('.*Not all inputs were used.*unknown_property.*', str(cm.exception)),
            'Exception message: {0}'.format(str(cm.exception)))

    def test_deserialize_dog_discard_unknown_properties(self):
        """ deserialize str, Dog) with unknown properties, discard unknown properties """
        config = Configuration(discard_unknown_keys=True)
        api_client = petstore_api.ApiClient(config)
        data = {
            'dog': {
                "class_name": "Dog",
                "color": "black",
                "breed": "husky",
                "unknown_property": "a-value",
                "more-unknown": [
                    "a"
                ]
            }
        }
        # The 'unknown_property' is undeclared, which would normally raise an exception, but
        # when discard_unknown_keys is set to True, the unknown properties are discarded.
        response = MockResponse(data=json.dumps(data))
        deserialized = api_client.deserialize(response,
            ({str: (petstore_api.Dog,)},), True)
        self.assertTrue(isinstance(deserialized, dict))
        self.assertTrue(isinstance(deserialized['dog'], petstore_api.Dog))

    def test_deserialize_cat_do_not_discard_unknown_properties(self):
        """ deserialize str, Cat) with unknown properties, strict validation is enabled """
        config = Configuration(discard_unknown_keys=False)
        api_client = petstore_api.ApiClient(config)
        data = {
            'cat': {
                "class_name": "Cat",
                "color": "black",
                "declawed": True,
                "dynamic-property": 12345,
            }
        }
        response = MockResponse(data=json.dumps(data))

        # Deserializing with strict validation does not raise an exception because the even though
        # the 'dynamic-property' is undeclared, the 'Cat' schema defines the additionalProperties
        # attribute.
        deserialized = api_client.deserialize(response,
            ({str: (petstore_api.Cat,)},), True)
        self.assertTrue(isinstance(deserialized, dict))
        self.assertTrue(isinstance(deserialized['cat'], petstore_api.Cat))
        self.assertEqual(deserialized['cat']['color'], 'black')

    def test_deserialize_cat_discard_unknown_properties(self):
        """ deserialize str, Cat) with unknown properties, discard unknown properties """
        config = Configuration(discard_unknown_keys=True)
        api_client = petstore_api.ApiClient(config)
        data = {
            'cat': {
                "class_name": "Cat",
                "color": "black",
                "declawed": True,
                "unknown_property": "a-value",
                "more-unknown": [
                    "a"
                ]
            }
        }
        # The 'unknown_property' is undeclared, which would normally raise an exception, but
        # when discard_unknown_keys is set to True, the unknown properties are discarded.
        response = MockResponse(data=json.dumps(data))
        deserialized = api_client.deserialize(response,
            ({str: (petstore_api.Cat,)},), True)
        self.assertTrue(isinstance(deserialized, dict))
        self.assertTrue(isinstance(deserialized['cat'], petstore_api.Cat))

