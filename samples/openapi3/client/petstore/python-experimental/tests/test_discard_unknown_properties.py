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
            "class_name": "Dog",
            "color": "black",
            "breed": "husky",
            "unknown_property": "a-value"
        }
        response = MockResponse(data=json.dumps(data))

        # Deserializing with strict validation raises an exception because the 'unknown_property'
        # is undeclared.
        with self.assertRaises(petstore_api.ApiValueError) as cm:
            deserialized = api_client.deserialize(response, ((petstore_api.Dog),), True)
        self.assertTrue(re.match('.*Not all inputs were used.*unknown_property.*', str(cm.exception)),
            'Exception message: {0}'.format(str(cm.exception)))

    def test_deserialize_dog_discard_unknown_properties(self):
        """ deserialize str, Dog) with unknown properties, discard unknown properties """
        config = Configuration(discard_unknown_keys=True)
        api_client = petstore_api.ApiClient(config)
        data = {
            "class_name": "Dog",
            "color": "black",
            "breed": "husky",
            "unknown_property": "a-value",
            "more-unknown": [
                "a"
            ]
        }
        # The 'unknown_property' is undeclared, which would normally raise an exception, but
        # when discard_unknown_keys is set to True, the unknown properties are discarded.
        response = MockResponse(data=json.dumps(data))
        deserialized = api_client.deserialize(response, ((petstore_api.Dog),), True)
        self.assertTrue(isinstance(deserialized, petstore_api.Dog))
        # Check the 'unknown_property' and 'more-unknown' properties are not present in the
        # output.
        self.assertIn("breed", deserialized.to_dict().keys())
        self.assertNotIn("unknown_property", deserialized.to_dict().keys())
        self.assertNotIn("more-unknown", deserialized.to_dict().keys())

    def test_deserialize_cat_do_not_discard_unknown_properties(self):
        """ deserialize str, Cat) with unknown properties, strict validation is enabled """
        config = Configuration(discard_unknown_keys=False)
        api_client = petstore_api.ApiClient(config)
        data = {
            "class_name": "Cat",
            "color": "black",
            "declawed": True,
            "dynamic-property": 12345,
        }
        response = MockResponse(data=json.dumps(data))

        # Deserializing with strict validation does not raise an exception because the even though
        # the 'dynamic-property' is undeclared, the 'Cat' schema defines the additionalProperties
        # attribute.
        deserialized = api_client.deserialize(response, ((petstore_api.Cat),), True)
        self.assertTrue(isinstance(deserialized, petstore_api.Cat))
        self.assertIn('color', deserialized.to_dict())
        self.assertEqual(deserialized['color'], 'black')

    def test_deserialize_cat_discard_unknown_properties(self):
        """ deserialize str, Cat) with unknown properties.
        Request to discard unknown properties, but Cat is composed schema
        with one inner schema that has 'additionalProperties' set to true. """
        config = Configuration(discard_unknown_keys=True)
        api_client = petstore_api.ApiClient(config)
        data = {
            "class_name": "Cat",
            "color": "black",
            "declawed": True,
            "my_additional_property": 123,
        }
        # The 'my_additional_property' is undeclared, but 'Cat' has a 'Address' type through
        # the allOf: [ $ref: '#/components/schemas/Address' ].
        response = MockResponse(data=json.dumps(data))
        deserialized = api_client.deserialize(response, ((petstore_api.Cat),), True)
        self.assertTrue(isinstance(deserialized, petstore_api.Cat))
        # Check the 'unknown_property' and 'more-unknown' properties are not present in the
        # output.
        self.assertIn("declawed", deserialized.to_dict().keys())
        self.assertIn("my_additional_property", deserialized.to_dict().keys())

