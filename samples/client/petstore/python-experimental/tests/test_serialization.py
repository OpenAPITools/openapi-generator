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
    outer_enum,
    outer_number,
    string_boolean_map,
)
from petstore_api.model_utils import (
    file_type,
    int,
    model_to_dict,
    str,
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
            outer_enum.OuterEnum.allowed_values[('value',)]["PLACED"])
        outer_enum_val = outer_enum.OuterEnum(value)

        source = enum_test.EnumTest(
            enum_string="UPPER",
            enum_string_required="lower",
            enum_integer=1,
            enum_number=1.1,
            outer_enum=outer_enum_val
        )

        result = {
            'enum_test': {
                "enum_string": "UPPER",
                "enum_string_required": "lower",
                "enum_integer": 1,
                "enum_number": 1.1,
                "outerEnum": "placed"
            }
        }
        serialized = self.serialize({"enum_test": source})

        self.assertEqual(result, serialized)
