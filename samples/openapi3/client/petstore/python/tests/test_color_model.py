# coding: utf-8

# flake8: noqa

import json
import os
import time
import unittest

from pydantic import ValidationError
import pytest

import petstore_api
from petstore_api import InnerDictWithProperty


class ColorModelTests(unittest.TestCase):
    def test_oneof_schema_2_validator(self):
        array_of_integers = [12, 34, 56]
        c = petstore_api.Color()

        try:
            c.oneof_schema_2_validator = array_of_integers
            self.fail(f"Should have failed: {c.oneof_schema_2_validator}")
        except ValueError as e:
            self.assertTrue("List should have at least 4 items after validation, not 3" in str(e))

    def test_new_null(self):
        c = petstore_api.Color()
        self.assertEqual("null", c.to_json())
        self.assertEqual(None, c.actual_instance)

    def test_oneof_validator(self):
        array_of_integers = [12, 34, 56]
        c = petstore_api.Color()

        # no error should be thrown
        c.oneof_schema_1_validator = array_of_integers
        c.actual_instance = array_of_integers
        c.actual_instance = None

        # test the oneof schema validator with invalid input 
        array_of_integers = [12,34,56120938]
        try:
            c.oneof_schema_1_validator = array_of_integers
            self.fail(f"Should have failed: {c.oneof_schema_1_validator}")
        except ValueError as e:
            self.assertTrue("Input should be less than or equal to 255" in str(e))

        try:
            c.actual_instance = array_of_integers
            self.fail(f"Should have failed: {c.actual_instance}")
        except ValueError as e:
            self.assertTrue("Input should be less than or equal to 255" in str(e))

    def test_from_json(self):
        json_str = '[12,34,56]'
        c = petstore_api.Color.from_json(json_str)
        self.assertEqual(c.actual_instance, [12, 34, 56])

        try:
            c = petstore_api.Color.from_json('[2342112,0,0,0]')
        except ValueError as e:
            self.assertTrue("Input should be less than or equal to 255" in str(e))

    def test_to_json(self):
        json_str = '[12,34,56]'
        c = petstore_api.Color.from_json(json_str)
        self.assertEqual(c.to_json(), "[12, 34, 56]")

    def test_to_dict(self):
        json_str = '[12,34,56]'
        c = petstore_api.Color.from_json(json_str)
        self.assertEqual(c.to_dict(), [12, 34, 56])

    def test_nullable(self):
        c = petstore_api.Color.from_json(None)
        self.assertEqual(c.actual_instance, None)

    def test_constraint_rgb(self):
        rgb = [128, 128, 128]
        color = petstore_api.Color(oneof_schema_1_validator=rgb)
        self.assertEqual(rgb, color.oneof_schema_1_validator)

        try:
            petstore_api.Color(oneof_schema_2_validator=rgb)
            self.fail("invalid validation")
        except ValidationError as e:
            self.assertIn("List should have at least 4 items after validation, not 3", str(e))

    def test_constraint_rgba(self):
        rgba = [128, 128, 128, 128]
        color = petstore_api.Color(oneof_schema_2_validator=rgba)
        self.assertEqual(rgba, color.oneof_schema_2_validator)

        try:
            petstore_api.Color(oneof_schema_1_validator=rgba)
            self.fail("invalid validation")
        except ValidationError as e:
            self.assertIn("List should have at most 3 items after validation, not 4", str(e))

    def test_constraint_hex(self):
        hex_color = "#00FF00"
        color = petstore_api.Color(oneof_schema_3_validator=hex_color)
        self.assertEqual(hex_color, color.oneof_schema_3_validator)

        try:
            petstore_api.Color(oneof_schema_3_validator="too long string")
            self.fail("invalid validation")
        except ValidationError as e:
            self.assertIn("String should have at most 7 characters", str(e))


class AnyOfColorModelTests(unittest.TestCase):
    def test_new_null(self):
        c = petstore_api.AnyOfColor()
        self.assertEqual("null", c.to_json())
        self.assertEqual(None, c.actual_instance)

    def test_anyof_array_of_integers(self):
        array_of_integers = [12,34,56]
        c = petstore_api.AnyOfColor()

        # no error should be thrown
        c.anyof_schema_1_validator = array_of_integers
        c.actual_instance = array_of_integers

        # test the oneof schema validator with invalid input 
        array_of_integers = [12,34,56120938]
        try:
            c.anyof_schema_1_validator = array_of_integers
            self.fail(f"Should have failed: {c.anyof_schema_1_validator}")
        except ValueError as e:
            self.assertIn("Input should be less than or equal to 255", str(e))

        try:
            c.actual_instance = array_of_integers
            self.fail(f"Should have failed: {c.actual_instance}")
        except ValueError as e:
            self.assertIn("Input should be less than or equal to 255", str(e))

    def test_from_json(self):
        json_str = '[12,34,56]'
        c = petstore_api.AnyOfColor.from_json(json_str)
        self.assertEqual(c.actual_instance, [12, 34,56])

        try:
            c = petstore_api.AnyOfColor.from_json('[2342112,0,0,0]')
            self.fail(f"Should have failed: {c.from_json}")
        except ValueError as e:
            self.assertIn("Input should be less than or equal to 255", str(e))
