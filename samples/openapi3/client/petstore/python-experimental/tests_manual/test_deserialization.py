# coding: utf-8

# flake8: noqa

"""
Run the tests.
$ pip install nose (optional)
$ cd OpenAPIPetstore-python
$ nosetests -v
"""
from collections import namedtuple
from decimal import Decimal
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
        self.assertEqual(deserialized.shapeType, 'Triangle')
        self.assertEqual(deserialized.triangleType, 'EquilateralTriangle')

        # invalid quadrilateralType, second discriminator value
        data = {
            'shapeType': 'Quadrilateral',
            'quadrilateralType': 'Triangle',
        }
        response = MockResponse(data=json.dumps(data))

        err_msg = (r"Invalid inputs given to generate an instance of .+?Quadrilateral"
            r".+?None of the oneOf schemas matched the input data."
        )
        with self.assertRaisesRegex(petstore_api.ApiValueError, err_msg):
            self.deserialize(response, (shape.Shape,), True)

    def test_deserialize_animal(self):
        """
        deserialize Animal to a Dog instance
        Animal uses a discriminator which has a map built of child classes
        that inherrit from Animal
        This is the swagger (v2) way of doing something like oneOf composition
        """
        data = {
            'className': 'Dog',
            'color': 'white',
            'breed': 'Jack Russel Terrier'
        }
        response = MockResponse(data=json.dumps(data))

        deserialized = self.deserialize(response, (animal.Animal,), True)
        self.assertTrue(isinstance(deserialized, dog.Dog))
        self.assertEqual(deserialized.className, 'Dog')
        self.assertEqual(deserialized.color, 'white')
        self.assertEqual(deserialized.breed, 'Jack Russel Terrier')

    def test_regex_constraint(self):
        """
        Test regex pattern validation.
        """

        # Test with valid regex pattern.
        inst = apple.Apple(
            cultivar="Akane"
        )
        assert isinstance(inst, apple.Apple)

        inst = apple.Apple(
            cultivar="Golden Delicious",
            origin="cHiLe"
        )
        assert isinstance(inst, apple.Apple)

        # Test with invalid regex pattern.
        err_regex = r"Invalid value `.+?`, must match regular expression `.+?` at \('args\[0\]', 'cultivar'\)"
        with self.assertRaisesRegex(
            petstore_api.ApiValueError,
            err_regex
        ):
            inst = apple.Apple(
                cultivar="!@#%@$#Akane"
            )

        err_regex = r"Invalid value `.+?`, must match regular expression `.+?` at \('args\[0\]', 'origin'\)"
        with self.assertRaisesRegex(
            petstore_api.ApiValueError,
            err_regex
        ):
            inst = apple.Apple(
                cultivar="Golden Delicious",
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
        self.assertEqual(deserialized.hasBaleen.value, has_baleen)
        self.assertEqual(deserialized.hasTeeth.value, has_teeth)
        self.assertEqual(deserialized.className, class_name)

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
        self.assertEqual(deserialized.className, class_name)

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
        self.assertTrue(isinstance(deserialized.lengthCm, Decimal))
        self.assertEqual(deserialized.lengthCm, 3.1415)

        """
        Float value is serialized without decimal point
        The client receive it as an integer, which work because Banana.lengthCm is type number without format
        Which accepts int AND float
        """
        data = {
          'lengthCm': 3
        }
        response = MockResponse(data=json.dumps(data))
        deserialized = self.deserialize(response, (banana.Banana,), True)
        self.assertTrue(isinstance(deserialized, banana.Banana))
        self.assertTrue(isinstance(deserialized.lengthCm, Decimal))
        self.assertEqual(deserialized.lengthCm, 3)

    def test_deserialize_fruit_null_value(self):
        """
        deserialize fruit with null value.
        fruitReq is a oneOf composed schema model with discriminator, including 'null' type.
        """

        # Unmarshal 'null' value
        data = None
        response = MockResponse(data=json.dumps(data))
        deserialized = self.deserialize(response, (fruit_req.FruitReq,), True)
        self.assertTrue(isinstance(deserialized, fruit_req.FruitReq))
        self.assertIsNone(deserialized.value)

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
        self.assertTrue(isinstance(deserialized, dog.Dog))
        self.assertEqual(deserialized.className, 'Dog')
        self.assertEqual(deserialized.color, 'brown')
        self.assertEqual(deserialized.breed, 'golden retriever')
        self.assertEqual(deserialized.group, 'Terrier Group')
        self.assertEqual(deserialized.size, 'medium')

        # The 'zebra' schema allows additional properties by explicitly setting
        # additionalProperties: true.
        # This is equivalent to 'additionalProperties' not being present.
        data = {
            'className': 'zebra',
            'type': 'plains',
            # Below are additional, undeclared properties
            'group': 'abc',
            'size': 3,
            'p1': True,
            'p2': [ 'a', 'b', 123],
        }
        response = MockResponse(data=json.dumps(data))
        deserialized = self.deserialize(response, (mammal.Mammal,), True)
        self.assertTrue(isinstance(deserialized, zebra.Zebra))
        self.assertEqual(deserialized.className, 'zebra')
        self.assertEqual(deserialized.type, 'plains')
        self.assertEqual(deserialized.p1.value, True)

        # The 'bananaReq' schema disallows additional properties by explicitly setting
        # additionalProperties: false
        with self.assertRaisesRegex(
            petstore_api.exceptions.ApiTypeError,
            r"BananaReq was passed 1 invalid argument: \['unknown-group'\]"
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

    def test_deserialize_with_additional_properties_and_reference(self):
        """
        Deserialize data with schemas that has the additionalProperties keyword
        and the schema is specified as a reference ($ref).
        """
        data = {
            'mainShape': {
                'shapeType': 'Triangle',
                'triangleType': 'EquilateralTriangle',
            },
            'shapes': [
                {
                    'shapeType': 'Triangle',
                    'triangleType': 'IsoscelesTriangle',
                },
                {
                    'shapeType': 'Quadrilateral',
                    'quadrilateralType': 'ComplexQuadrilateral',
                },
            ],
            'an_additional_prop': {
                'lengthCm': 4,
                'color': 'yellow'
            }
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
        self.assertEqual(number, number_val)