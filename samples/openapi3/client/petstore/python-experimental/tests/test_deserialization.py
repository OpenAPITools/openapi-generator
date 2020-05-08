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

import petstore_api


MockResponse = namedtuple('MockResponse', 'data')


class DeserializationTests(unittest.TestCase):

    def setUp(self):
        self.api_client = petstore_api.ApiClient()
        self.deserialize = self.api_client.deserialize

    def test_deserialize_shape(self):
        """

        deserialize Shape to an instance of:
        - EquilateralTriangle
        - IsoscelesTriangle
        - IsoscelesTriangle
        - ScaleneTriangle
        - ComplexQuadrilateral
        - SimpleQuadrilateral
        by traveling through 2 discriminators
        """
        shape_type, triangle_type  = ['Triangle', 'EquilateralTriangle']
        data = {
            'shapeType': shape_type,
            'triangleType': triangle_type,
        }
        response = MockResponse(data=json.dumps(data))

        deserialized = self.deserialize(response, (petstore_api.Shape,), True)
        self.assertTrue(isinstance(deserialized, petstore_api.EquilateralTriangle))
        self.assertEqual(deserialized.shape_type, shape_type)
        self.assertEqual(deserialized.triangle_type, triangle_type)

        # invalid second discriminator value
        shape_type, quadrilateral_type  = ['Quadrilateral', 'Triangle']
        data = {
            'shapeType': shape_type,
            'quadrilateralType': quadrilateral_type,
        }
        response = MockResponse(data=json.dumps(data))

        err_msg = ("Cannot deserialize input data due to invalid discriminator "
            "value. The OpenAPI document has no mapping for discriminator "
            "property '{}'='{}' at path: ()"
        )
        with self.assertRaisesRegexp(
            petstore_api.ApiValueError,
            err_msg.format("quadrilateralType", "Triangle")
        ):
            self.deserialize(response, (petstore_api.Shape,), True)

    def test_deserialize_animal(self):
        """
        deserialize Animal to a Dog instance
        Animal uses a discriminator which has a map built of child classes
        that inherrit from Animal
        This is the swagger (v2) way of doing something like oneOf composition
        """
        class_name = 'Dog'
        color = 'white'
        breed = 'Jack Russel Terrier'
        data = {
            'className': class_name,
            'color': color,
            'breed': breed
        }
        response = MockResponse(data=json.dumps(data))

        deserialized = self.deserialize(response, (petstore_api.Animal,), True)
        self.assertTrue(isinstance(deserialized, petstore_api.Dog))
        self.assertEqual(deserialized.class_name, class_name)
        self.assertEqual(deserialized.color, color)
        self.assertEqual(deserialized.breed, breed)

    def test_deserialize_mammal(self):
        """
        deserialize mammal
        mammal is a oneOf composed schema model with discriminator
        """

        # whale test
        has_baleen = True
        has_teeth = False
        class_name = 'whale'
        data = {
            'hasBaleen': has_baleen,
            'hasTeeth': has_teeth,
            'className': class_name
        }
        response = MockResponse(data=json.dumps(data))
        deserialized = self.deserialize(response, (petstore_api.Mammal,), True)
        self.assertTrue(isinstance(deserialized, petstore_api.Whale))
        self.assertEqual(deserialized.has_baleen, has_baleen)
        self.assertEqual(deserialized.has_teeth, has_teeth)
        self.assertEqual(deserialized.class_name, class_name)

        # zebra test
        zebra_type = 'plains'
        class_name = 'zebra'
        data = {
            'type': zebra_type,
            'className': class_name
        }
        response = MockResponse(data=json.dumps(data))
        deserialized = self.deserialize(response, (petstore_api.Mammal,), True)
        self.assertTrue(isinstance(deserialized, petstore_api.Zebra))
        self.assertEqual(deserialized.type, zebra_type)
        self.assertEqual(deserialized.class_name, class_name)

    def test_deserialize_fruit_null_value(self):
        """
        deserialize fruit with null value.
        fruitReq is a oneOf composed schema model with discriminator, including 'null' type.
        """

        # Unmarshal 'null' value
        data = None
        response = MockResponse(data=json.dumps(data))
        deserialized = self.deserialize(response, (petstore_api.FruitReq, type(None)), True)
        self.assertEqual(type(deserialized), type(None))

        inst = petstore_api.FruitReq(None)
        self.assertIsNone(inst)
