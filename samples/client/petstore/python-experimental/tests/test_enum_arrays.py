# coding: utf-8

# flake8: noqa

"""
Run the tests.
$ pip install nose (optional)
$ cd petstore_api-python
$ nosetests -v
"""

import os
import time
import unittest

import petstore_api

from petstore_api.exceptions import (
    ApiTypeError,
    ApiKeyError,
    ApiValueError,
)

class EnumArraysTests(unittest.TestCase):

    def test_enumarrays_init(self):
      #
      # Check various combinations of valid values.
      #
      fish_or_crab = petstore_api.EnumArrays(just_symbol=">=")
      self.assertEqual(fish_or_crab.just_symbol, ">=")
      # if optional property is unset we raise an exception
      with self.assertRaises(AttributeError) as exc:
          self.assertEqual(fish_or_crab.array_enum, None)

      fish_or_crab = petstore_api.EnumArrays(just_symbol="$", array_enum=["fish"])
      self.assertEqual(fish_or_crab.just_symbol, "$")
      self.assertEqual(fish_or_crab.array_enum, ["fish"])

      fish_or_crab = petstore_api.EnumArrays(just_symbol=">=", array_enum=["fish"])
      self.assertEqual(fish_or_crab.just_symbol, ">=")
      self.assertEqual(fish_or_crab.array_enum, ["fish"])

      fish_or_crab = petstore_api.EnumArrays(just_symbol="$", array_enum=["crab"])
      self.assertEqual(fish_or_crab.just_symbol, "$")
      self.assertEqual(fish_or_crab.array_enum, ["crab"])


      #
      # Check if setting invalid values fails
      #
      with self.assertRaises(ApiValueError) as exc:
        fish_or_crab = petstore_api.EnumArrays(just_symbol="<=")

      with self.assertRaises(ApiValueError) as exc:
        fish_or_crab = petstore_api.EnumArrays(just_symbol="$", array_enum=["dog"])

      with self.assertRaises(ApiTypeError) as exc:
        fish_or_crab = petstore_api.EnumArrays(just_symbol=["$"], array_enum=["crab"])


    def test_enumarrays_setter(self):

      #
      # Check various combinations of valid values
      #
      fish_or_crab = petstore_api.EnumArrays()

      fish_or_crab.just_symbol = ">="
      self.assertEqual(fish_or_crab.just_symbol, ">=")

      fish_or_crab.just_symbol = "$"
      self.assertEqual(fish_or_crab.just_symbol, "$")

      fish_or_crab.array_enum = []
      self.assertEqual(fish_or_crab.array_enum, [])

      fish_or_crab.array_enum = ["fish"]
      self.assertEqual(fish_or_crab.array_enum, ["fish"])

      fish_or_crab.array_enum = ["fish", "fish", "fish"]
      self.assertEqual(fish_or_crab.array_enum, ["fish", "fish", "fish"])

      fish_or_crab.array_enum = ["crab"]
      self.assertEqual(fish_or_crab.array_enum, ["crab"])

      fish_or_crab.array_enum = ["crab", "fish"]
      self.assertEqual(fish_or_crab.array_enum, ["crab", "fish"])

      fish_or_crab.array_enum = ["crab", "fish", "crab", "fish"]
      self.assertEqual(fish_or_crab.array_enum, ["crab", "fish", "crab", "fish"])

      #
      # Check if setting invalid values fails
      #
      fish_or_crab = petstore_api.EnumArrays()
      with self.assertRaises(ApiValueError) as exc:
          fish_or_crab.just_symbol = "!="

      with self.assertRaises(ApiTypeError) as exc:
        fish_or_crab.just_symbol = ["fish"]

      with self.assertRaises(ApiValueError) as exc:
        fish_or_crab.array_enum = ["cat"]

      with self.assertRaises(ApiValueError) as exc:
        fish_or_crab.array_enum = ["fish", "crab", "dog"]

      with self.assertRaises(ApiTypeError) as exc:
        fish_or_crab.array_enum = "fish"


    def test_todict(self):
      #
      # Check if dictionary serialization works
      #
      dollar_fish_crab_dict = {
        'just_symbol': "$",
        'array_enum': ["fish", "crab"]
      }

      dollar_fish_crab = petstore_api.EnumArrays(
        just_symbol="$", array_enum=["fish", "crab"])

      self.assertEqual(dollar_fish_crab_dict, dollar_fish_crab.to_dict())

      #
      # Sanity check for different arrays
      #
      dollar_crab_fish_dict = {
        'just_symbol': "$",
        'array_enum': ["crab", "fish"]
      }

      dollar_fish_crab = petstore_api.EnumArrays(
        just_symbol="$", array_enum=["fish", "crab"])

      self.assertNotEqual(dollar_crab_fish_dict, dollar_fish_crab.to_dict())


    def test_equals(self):
      #
      # Check if object comparison works
      #
      fish1 = petstore_api.EnumArrays(just_symbol="$", array_enum=["fish"])
      fish2 = petstore_api.EnumArrays(just_symbol="$", array_enum=["fish"])
      self.assertEqual(fish1, fish2)

      fish = petstore_api.EnumArrays(just_symbol="$", array_enum=["fish"])
      crab = petstore_api.EnumArrays(just_symbol="$", array_enum=["crab"])
      self.assertNotEqual(fish, crab)

      dollar = petstore_api.EnumArrays(just_symbol="$")
      greater = petstore_api.EnumArrays(just_symbol=">=")
      self.assertNotEqual(dollar, greater)
