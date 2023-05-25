# coding: utf-8

# flake8: noqa

"""
Run the tests.
$ pip install -U pytest
$ cd petstore_api-python
$ pytest
"""

import os
import six
import sys
import unittest

import petstore_api
from petstore_api.rest import ApiException
from pydantic import BaseModel, ValidationError

from .util import id_gen


class ApiExceptionTests(unittest.TestCase):

    def setUp(self):
        self.api_client = petstore_api.ApiClient()
        self.pet_api = petstore_api.PetApi(self.api_client)
        self.setUpModels()

    def setUpModels(self):
        self.category = petstore_api.Category(name="dog")
        self.category.id = id_gen()
        self.category.name = "dog"
        self.tag = petstore_api.Tag()
        self.tag.id = id_gen()
        self.tag.name = "blank"
        self.pet = petstore_api.Pet(name="hello kity", photo_urls=["http://foo.bar.com/1", "http://foo.bar.com/2"])
        self.pet.id = id_gen()
        self.pet.status = "sold"
        self.pet.category = self.category
        self.pet.tags = [self.tag]

    def test_set_param_validation(self):
        try:
            self.pet_api.find_pets_by_tags(["a", "a"])
        except ValidationError as e:
            self.assertTrue("the list has duplicated items" in str(e))

    def test_required_param_validation(self):
        try:
            self.pet_api.get_pet_by_id()
        except ValidationError as e:
            self.assertEqual(str(e), "1 validation error for GetPetById\n"
                                     "pet_id\n"
                                     "  field required (type=value_error.missing)")

    def test_integer_validation(self):
        try:
            self.pet_api.get_pet_by_id("123")
        except ValidationError as e:
            self.assertEqual(str(e), "1 validation error for GetPetById\n"
                                     "pet_id\n"
                                     "  value is not a valid integer (type=type_error.integer)")

    def test_string_enum_validation(self):
        try:
            self.pet_api.find_pets_by_status(["Cat"])
        except ValidationError as e:
            self.assertEqual(str(e), "1 validation error for FindPetsByStatus\n"
                                     "status -> 0\n"
                                     "  unexpected value; permitted: 'available', 'pending', 'sold' ("
                                     "type=value_error.const; given=Cat; permitted=('available', 'pending', 'sold'))")

    def checkRaiseRegex(self, expected_exception, expected_regex):
        return self.assertRaisesRegex(expected_exception, expected_regex)

    def checkRegex(self, text, expected_regex):
        return self.assertRegex(text, expected_regex)
