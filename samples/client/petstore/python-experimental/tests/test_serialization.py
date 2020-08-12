# coding: utf-8

# flake8: noqa

"""
Run the tests.
$ pip install nose (optional)
$ cd OpenAPIPetstore-python
$ nosetests -v
"""
from collections import namedtuple
import json
import os
import time
import unittest
import datetime

import six

import petstore_api

from petstore_api.exceptions import (
    ApiTypeError,
    ApiKeyError,
    ApiValueError,
)
from petstore_api.model import (
    enum_test,
    pet,
    animal,
    dog,
    parent_pet,
    child_lizard,
    category,
    string_enum,
    string_boolean_map,
)
from petstore_api.model_utils import (
    file_type,
    model_to_dict,
)

from petstore_api.rest import RESTResponse

MockResponse = namedtuple('MockResponse', 'data')

class SerializationTests(unittest.TestCase):

    def setUp(self):
        self.api_client = petstore_api.ApiClient()
        self.serialize = self.api_client.sanitize_for_serialization

    def test_enum_test(self):
        """ serialize dict(str, Enum_Test) """
        value = (
            string_enum.StringEnum.allowed_values[('value',)]["PLACED"])
        string_enum_val = string_enum.StringEnum(value)

        source = enum_test.EnumTest(
            enum_string="UPPER",
            enum_string_required="lower",
            enum_integer=1,
            enum_number=1.1,
            string_enum=string_enum_val
        )

        result = {
            'enum_test': {
                "enum_string": "UPPER",
                "enum_string_required": "lower",
                "enum_integer": 1,
                "enum_number": 1.1,
                "stringEnum": "placed"
            }
        }
        serialized = self.serialize({"enum_test": source})

        self.assertEqual(result, serialized)
