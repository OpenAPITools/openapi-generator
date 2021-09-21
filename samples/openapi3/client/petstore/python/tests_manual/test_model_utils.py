# coding: utf-8

# flake8: noqa

"""
Run the tests.
$ pip install nose (optional)
$ cd OpenAPIPetstore-python
$ nosetests -v
"""

import unittest

import petstore_api
from petstore_api.model.array_test import ArrayTest
from petstore_api.model.read_only_first import ReadOnlyFirst
from petstore_api.model_utils import model_to_dict

class ModelUtilsTests(unittest.TestCase):

    def test_model_to_dict(self):
        model = ArrayTest(
            array_of_string=["foo", "bar", "baz"],
            array_array_of_integer=[[1], [2], [3]],
            array_of_free_form_object=[
                {"foo": "bar", "baz": 42},
                {"qux": "quux", "quuz": 42.0}
            ],
            array_array_of_model=[[ReadOnlyFirst()]]
        )

        model_to_dict(model)
