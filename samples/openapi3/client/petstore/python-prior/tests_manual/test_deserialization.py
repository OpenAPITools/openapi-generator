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
from petstore_api.model import (
    shape,
    equilateral_triangle,
    animal,
    dog,
    apple,
    mammal,
    whale,
    zebra,
    banana,
    fruit_req,
    drawing,
    banana_req,
    number_with_validations,
)


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

        deserialized = self.deserialize(response, (shape.Shape,), True)
        self.assertTrue(isinstance(deserialized, equilateral_triangle.EquilateralTriangle))
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
        with self.assertRaisesRegex(
            petstore_api.ApiValueError,
            err_msg.format("quadrilateralType", "Triangle")
        ):
            self.deserialize(response, (shape.Shape,), True)

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

        deserialized = self.deserialize(response, (animal.Animal,), True)
        self.assertTrue(isinstance(deserialized, dog.Dog))
        self.assertEqual(deserialized.class_name, class_name)
        self.assertEqual(deserialized.color, color)
        self.assertEqual(deserialized.breed, breed)

    def test_regex_constraint(self):
        """
        Test regex pattern validation.
        """

        # Test with valid regex pattern.
        inst = apple.Apple(
            cultivar="Akane"
        )
        assert isinstance(inst, apple.Apple)

        # Test with invalid regex pattern in cultivar
        err_msg = ("Invalid value for `{}`, must match regular expression `{}`$")
        with self.assertRaisesRegex(
            petstore_api.ApiValueError,
            err_msg.format("cultivar", "[^`]*")
        ):
            inst = apple.Apple(
                cultivar="!@#%@$#Akane"
            )

        # Test with invalid regex pattern in origin
        err_msg = ("Invalid value for `{}`, must match regular expression `{}` with flags")
        with self.assertRaisesRegex(
            petstore_api.ApiValueError,
            err_msg.format("origin", "[^`]*")
        ):
            inst = apple.Apple(
                cultivar="Akane",
                origin="!@#%@$#Chile"
            )

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
        deserialized = self.deserialize(response, (mammal.Mammal,), True)
        self.assertTrue(isinstance(deserialized, whale.Whale))
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
        deserialized = self.deserialize(response, (mammal.Mammal,), True)
        self.assertTrue(isinstance(deserialized, zebra.Zebra))
        self.assertEqual(deserialized.type, zebra_type)
        self.assertEqual(deserialized.class_name, class_name)

    def test_deserialize_float_value(self):
        """
        Deserialize floating point values.
        """
        data = {
          'lengthCm': 3.1415
        }
        response = MockResponse(data=json.dumps(data))
        deserialized = self.deserialize(response, (banana.Banana,), True)
        self.assertTrue(isinstance(deserialized, banana.Banana))
        self.assertEqual(deserialized.length_cm, 3.1415)

        # Float value is serialized without decimal point
        data = {
          'lengthCm': 3
        }
        response = MockResponse(data=json.dumps(data))
        deserialized = self.deserialize(response, (banana.Banana,), True)
        self.assertTrue(isinstance(deserialized, banana.Banana))
        self.assertEqual(deserialized.length_cm, 3.0)

    def test_deserialize_fruit_null_value(self):
        """
        deserialize fruit with null value.
        fruitReq is a oneOf composed schema model with discriminator, including 'null' type.
        """

        # Unmarshal 'null' value
        data = None
        response = MockResponse(data=json.dumps(data))
        deserialized = self.deserialize(response, (fruit_req.FruitReq, type(None)), True)
        self.assertEqual(type(deserialized), type(None))

        inst = fruit_req.FruitReq(None)
        self.assertIsNone(inst)

    def test_deserialize_with_additional_properties(self):
        """
        Deserialize data with schemas that have the additionalProperties keyword.
        Test conditions when additional properties are allowed, not allowed, have
        specific types...
        """

        # Dog is allOf with two child schemas.
        # The OAS document for Dog does not specify the 'additionalProperties' keyword,
        # which means that by default, the Dog schema must allow undeclared properties.
        # The additionalProperties keyword is used to control the handling of extra stuff,
        # that is, properties whose names are not listed in the properties keyword.
        # By default any additional properties are allowed.
        data = {
            'className': 'Dog',
            'color': 'brown',
            'breed': 'golden retriever',
            # Below are additional, undeclared properties.
            'group': 'Terrier Group',
            'size': 'medium',
        }
        response = MockResponse(data=json.dumps(data))
        deserialized = self.deserialize(response, (dog.Dog,), True)
        self.assertEqual(type(deserialized), dog.Dog)
        self.assertEqual(deserialized.class_name, 'Dog')
        self.assertEqual(deserialized.breed, 'golden retriever')

        # The 'zebra' schema allows additional properties by explicitly setting
        # additionalProperties: true.
        # This is equivalent to 'additionalProperties' not being present.
        data = {
            'class_name': 'zebra',
            'type': 'plains',
            # Below are additional, undeclared properties
            'group': 'abc',
            'size': 3,
            'p1': True,
            'p2': [ 'a', 'b', 123],
        }
        response = MockResponse(data=json.dumps(data))
        deserialized = self.deserialize(response, (mammal.Mammal,), True)
        self.assertEqual(type(deserialized), zebra.Zebra)
        self.assertEqual(deserialized.class_name, 'zebra')
        self.assertEqual(deserialized.type, 'plains')
        self.assertEqual(deserialized.p1, True)

        # The 'bananaReq' schema disallows additional properties by explicitly setting
        # additionalProperties: false
        err_msg = ("{} has no attribute '{}' at ")
        with self.assertRaisesRegex(
            petstore_api.exceptions.ApiAttributeError,
            err_msg.format("BananaReq", "unknown-group")
        ):
            data = {
                'lengthCm': 21.2,
                'sweet': False,
                # Below are additional, undeclared properties. They are not allowed,
                # an exception must be raised.
                'unknown-group': 'abc',
            }
            response = MockResponse(data=json.dumps(data))
            deserialized = self.deserialize(response, (banana_req.BananaReq,), True)
            self.assertEqual(type(deserialized), banana_req.BananaReq)
            self.assertEqual(deserialized.lengthCm, 21)
            self.assertEqual(deserialized.p1, True)

    def test_deserialize_with_additional_properties_and_reference(self):
        """
        Deserialize data with schemas that has the additionalProperties keyword
        and the schema is specified as a reference ($ref).
        """
        data = {
            'main_shape': {
                'shape_type': 'Triangle',
                'triangle_type': 'EquilateralTriangle',
            },
            'shapes': [
                {
                    'shape_type': 'Triangle',
                    'triangle_type': 'IsoscelesTriangle',
                },
                {
                    'shape_type': 'Quadrilateral',
                    'quadrilateral_type': 'ComplexQuadrilateral',
                },
            ],
        }
        response = MockResponse(data=json.dumps(data))
        deserialized = self.deserialize(response, (drawing.Drawing,), True)

    def test_deserialize_NumberWithValidations(self):
        """ deserialize NumberWithValidations """
        # make sure that an exception is thrown on an invalid type value
        with self.assertRaises(petstore_api.ApiTypeError):
            self.deserialize(
                MockResponse(data=json.dumps("test str")),
                (number_with_validations.NumberWithValidations,),
                True
            )

        # make sure that an exception is thrown on an invalid value
        with self.assertRaises(petstore_api.ApiValueError):
            self.deserialize(
                MockResponse(data=json.dumps(21.0)),
                (number_with_validations.NumberWithValidations,),
                True
            )

        # valid value works
        number_val = 11.0
        response = MockResponse(data=json.dumps(number_val))
        number = self.deserialize(response,
            (number_with_validations.NumberWithValidations,), True)
        self.assertTrue(isinstance(number, number_with_validations.NumberWithValidations))
        self.assertTrue(number.value == number_val)