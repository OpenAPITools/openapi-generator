# coding: utf-8

# flake8: noqa

"""
Run the tests.
$ pip install -U pytest
$ cd petstore_api-python
$ pytest
"""

import os
import time
import unittest

import petstore_api


class MapTestTests(unittest.TestCase):

    def test_maptest_init(self):
        #
        # Test MapTest construction with valid values
        #
        up_or_low_dict = {
            'UPPER': "UP",
            'lower': "low"
        }
#      map_enum_test = petstore_api.MapTest(map_of_enum_string=up_or_low_dict)
#
#      self.assertEqual(map_enum_test.map_of_enum_string, up_or_low_dict)
#
#      map_of_map_of_strings = {
#        'val1': 1,
#        'valText': "Text",
#        'valueDict': up_or_low_dict
#      }
#      map_enum_test = petstore_api.MapTest(map_map_of_string=map_of_map_of_strings)
#
#      self.assertEqual(map_enum_test.map_map_of_string, map_of_map_of_strings)
#
#      #
#      # Make sure that the init fails for invalid enum values
#      #
#      black_or_white_dict = {
#        'black': "UP", 
#        'white': "low"
#      }
#      try:
#        map_enum_test = petstore_api.MapTest(map_of_enum_string=black_or_white_dict)
#        self.assertTrue(0)
#      except ValueError:
#        self.assertTrue(1)
#
#    def test_maptest_setter(self):
#      #
#      # Check with some valid values
#      #
#      map_enum_test = petstore_api.MapTest()
#      up_or_low_dict = {
#        'UPPER': "UP", 
#        'lower': "low"
#      }
#      map_enum_test.map_of_enum_string = up_or_low_dict
#      self.assertEqual(map_enum_test.map_of_enum_string, up_or_low_dict)
#
#      #
#      # Check if the setter fails for invalid enum values
#      #
#      map_enum_test = petstore_api.MapTest()
#      black_or_white_dict = {
#        'black': "UP", 
#        'white': "low"
#      }
#      try:
#        map_enum_test.map_of_enum_string = black_or_white_dict
#      except ValueError:
#        self.assertEqual(map_enum_test.map_of_enum_string, None)
#
#    def test_todict(self):
#      #
#      # Check dictionary serialization
#      #
#      map_enum_test = petstore_api.MapTest()
#      up_or_low_dict = {
#        'UPPER': "UP", 
#        'lower': "low"
#      }
#      map_of_map_of_strings = {
#        'val1': 1,
#        'valText': "Text",
#        'valueDict': up_or_low_dict
#      }
#      indirect_map = {
#        'option1': True
#      }
#      direct_map = {
#        'option2': False
#      }
#      map_enum_test.map_of_enum_string = up_or_low_dict
#      map_enum_test.map_map_of_string = map_of_map_of_strings
#      map_enum_test.indirect_map = indirect_map
#      map_enum_test.direct_map = direct_map
#     
#      self.assertEqual(map_enum_test.map_of_enum_string, up_or_low_dict)
#      self.assertEqual(map_enum_test.map_map_of_string, map_of_map_of_strings)
#      self.assertEqual(map_enum_test.indirect_map, indirect_map)
#      self.assertEqual(map_enum_test.direct_map, direct_map)
#
#      expected_dict = {
#        'map_of_enum_string': up_or_low_dict,
#        'map_map_of_string': map_of_map_of_strings,
#        'indirect_map': indirect_map,
#        'direct_map': direct_map
#      }
#
#      self.assertEqual(map_enum_test.to_dict(), expected_dict)
