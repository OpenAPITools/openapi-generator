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
import typing
import unittest

import urllib3

import petstore_api
from petstore_api import api_client
from petstore_api.schemas import NoneClass


MockResponse = namedtuple('MockResponse', 'data')


class DeserializationTests(unittest.TestCase):
    json_content_type = 'application/json'
    json_content_type_headers = {'content-type': json_content_type}
    configuration = petstore_api.Configuration()

    @classmethod
    def __response(cls, data: typing.Any) -> urllib3.HTTPResponse:
        return urllib3.HTTPResponse(
            json.dumps(data).encode('utf-8'),
            headers=cls.json_content_type_headers
        )

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
        from petstore_api.model import shape, equilateral_triangle
        _response_for_200 = api_client.OpenApiResponse(
            content={
                self.json_content_type: api_client.MediaType(schema=shape.Shape),
            },
        )
        data = {
            'shapeType': 'Triangle',
            'triangleType': 'EquilateralTriangle',
        }
        response = self.__response(data)
        deserialized = _response_for_200.deserialize(response, self.configuration)
        body = deserialized.body
        self.assertTrue(isinstance(body, equilateral_triangle.EquilateralTriangle))
        self.assertEqual(body.shapeType, 'Triangle')
        self.assertEqual(body.triangleType, 'EquilateralTriangle')

        # invalid quadrilateralType, second discriminator value
        data = {
            'shapeType': 'Quadrilateral',
            'quadrilateralType': 'Triangle',
        }
        response = self.__response(data)

        err_msg = (
            r"Invalid discriminator value was passed in to Quadrilateral.quadrilateralType Only the values "
            r"\['ComplexQuadrilateral', 'SimpleQuadrilateral'\] are allowed at \('args\[0\]', 'quadrilateralType'\)"
        )
        with self.assertRaisesRegex(petstore_api.ApiValueError, err_msg):
            _response_for_200.deserialize(response, self.configuration)

    def test_deserialize_animal(self):
        """
        deserialize Animal to a Dog instance
        Animal uses a discriminator which has a map built of child classes
        that inherrit from Animal
        This is the swagger (v2) way of doing something like oneOf composition
        """
        from petstore_api.model import animal, dog
        _response_for_200 = api_client.OpenApiResponse(
            content={
                self.json_content_type: api_client.MediaType(schema=animal.Animal),
            },
        )
        data = {
            'className': 'Dog',
            'color': 'white',
            'breed': 'Jack Russel Terrier'
        }
        response = self.__response(data)
        deserialized = _response_for_200.deserialize(response, self.configuration)
        body = deserialized.body
        self.assertTrue(isinstance(body, dog.Dog))
        self.assertEqual(body.className, 'Dog')
        self.assertEqual(body.color, 'white')
        self.assertEqual(body.breed, 'Jack Russel Terrier')

    def test_regex_constraint(self):
        """
        Test regex pattern validation.
        """
        from petstore_api.model import apple

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
        from petstore_api.model import mammal, zebra, whale
        _response_for_200 = api_client.OpenApiResponse(
            content={
                self.json_content_type: api_client.MediaType(schema=mammal.Mammal),
            },
        )
        has_baleen = True
        has_teeth = False
        class_name = 'whale'
        data = {
            'hasBaleen': has_baleen,
            'hasTeeth': has_teeth,
            'className': class_name
        }
        response = self.__response(data)
        deserialized = _response_for_200.deserialize(response, self.configuration)
        body = deserialized.body
        self.assertTrue(isinstance(body, whale.Whale))
        self.assertEqual(bool(body.hasBaleen), has_baleen)
        self.assertEqual(bool(body.hasTeeth), has_teeth)
        self.assertEqual(body.className, class_name)

        # zebra test
        zebra_type = 'plains'
        class_name = 'zebra'
        data = {
            'type': zebra_type,
            'className': class_name
        }
        response = self.__response(data)
        deserialized = _response_for_200.deserialize(response, self.configuration)
        body = deserialized.body
        self.assertTrue(isinstance(body, zebra.Zebra))
        self.assertEqual(body.type, zebra_type)
        self.assertEqual(body.className, class_name)

    def test_deserialize_float_value(self):
        """
        Deserialize floating point values.
        """
        from petstore_api.model import banana
        _response_for_200 = api_client.OpenApiResponse(
            content={
                self.json_content_type: api_client.MediaType(schema=banana.Banana),
            },
        )
        data = {
          'lengthCm': 3.1415
        }
        response = self.__response(data)
        deserialized = _response_for_200.deserialize(response, self.configuration)
        body = deserialized.body
        self.assertTrue(isinstance(body, banana.Banana))
        self.assertTrue(isinstance(body.lengthCm, Decimal))
        self.assertEqual(body.lengthCm, 3.1415)

        """
        Float value is serialized without decimal point
        The client receive it as an integer, which work because Banana.lengthCm is type number without format
        Which accepts int AND float
        """
        data = {
          'lengthCm': 3
        }
        response = self.__response(data)
        deserialized = _response_for_200.deserialize(response, self.configuration)
        body = deserialized.body
        self.assertTrue(isinstance(body, banana.Banana))
        self.assertTrue(isinstance(body.lengthCm, Decimal))
        self.assertEqual(body.lengthCm, 3)

    def test_deserialize_fruit_null_value(self):
        """
        deserialize fruit with null value.
        fruitReq is a oneOf composed schema model with discriminator, including 'null' type.
        """
        from petstore_api.model import fruit_req
        _response_for_200 = api_client.OpenApiResponse(
            content={
                self.json_content_type: api_client.MediaType(schema=fruit_req.FruitReq),
            },
        )
        data = None
        response = self.__response(data)
        deserialized = _response_for_200.deserialize(response, self.configuration)
        self.assertTrue(isinstance(deserialized.body, fruit_req.FruitReq))
        self.assertTrue(isinstance(deserialized.body, NoneClass))

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
        from petstore_api.model import dog, mammal, zebra, banana_req
        data = {
            'className': 'Dog',
            'color': 'brown',
            'breed': 'golden retriever',
            # Below are additional, undeclared properties.
            'group': 'Terrier Group',
            'size': 'medium',
        }
        response = self.__response(data)
        _response_for_200 = api_client.OpenApiResponse(
            content={
                self.json_content_type: api_client.MediaType(schema=dog.Dog),
            },
        )
        deserialized = _response_for_200.deserialize(response, self.configuration)
        body = deserialized.body
        self.assertTrue(isinstance(body, dog.Dog))
        self.assertEqual(body.className, 'Dog')
        self.assertEqual(body.color, 'brown')
        self.assertEqual(body.breed, 'golden retriever')
        self.assertEqual(body.group, 'Terrier Group')
        self.assertEqual(body.size, 'medium')

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
            'p2': ['a', 'b', 123],
        }
        response = self.__response(data)
        _response_for_200 = api_client.OpenApiResponse(
            content={
                self.json_content_type: api_client.MediaType(schema=mammal.Mammal),
            },
        )
        deserialized = _response_for_200.deserialize(response, self.configuration)
        body = deserialized.body
        self.assertTrue(isinstance(body, zebra.Zebra))
        self.assertEqual(body.className, 'zebra')
        self.assertEqual(body.type, 'plains')
        self.assertEqual(bool(body.p1), True)

        # The 'bananaReq' schema disallows additional properties by explicitly setting
        # additionalProperties: false
        _response_for_200 = api_client.OpenApiResponse(
            content={
                self.json_content_type: api_client.MediaType(schema=banana_req.BananaReq),
            },
        )
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
            response = self.__response(data)
            _response_for_200.deserialize(response, self.configuration)

    def test_deserialize_with_additional_properties_and_reference(self):
        """
        Deserialize data with schemas that has the additionalProperties keyword
        and the schema is specified as a reference ($ref).
        """
        from petstore_api.model import drawing
        _response_for_200 = api_client.OpenApiResponse(
            content={
                self.json_content_type: api_client.MediaType(schema=drawing.Drawing),
            },
        )
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
        response = self.__response(data)
        _response_for_200.deserialize(response, self.configuration)

    def test_deserialize_NumberWithValidations(self):
        from petstore_api.model.number_with_validations import NumberWithValidations
        from petstore_api.api.fake_api_endpoints.number_with_validations import _response_for_200

        # make sure that an exception is thrown on an invalid type value
        with self.assertRaises(petstore_api.ApiTypeError):
            response = self.__response('test str')
            _response_for_200.deserialize(response, self.configuration)

        # make sure that an exception is thrown on an invalid value
        with self.assertRaises(petstore_api.ApiValueError):
            response = self.__response(21.0)
            _response_for_200.deserialize(response, self.configuration)

        # valid value works
        number_val = 11.0
        response = self.__response(number_val)
        response = _response_for_200.deserialize(response, self.configuration)
        self.assertTrue(isinstance(response.body, NumberWithValidations))
        self.assertEqual(response.body, number_val)

    def test_array_of_enums(self):
        from petstore_api.model.array_of_enums import ArrayOfEnums
        from petstore_api.api.fake_api_endpoints.array_of_enums import _response_for_200
        from petstore_api.model import string_enum
        data = ["placed", None]
        response = self.__response(data)
        deserialized = _response_for_200.deserialize(response, self.configuration)
        assert isinstance(deserialized.body, ArrayOfEnums)
        expected_results = ArrayOfEnums([string_enum.StringEnum(v) for v in data])
        assert expected_results == deserialized.body

    def test_multiple_of_deserialization(self):
        data = {
            'byte': '3',
            'date': '1970-01-01',
            'password': "abcdefghijkl",
            'integer': 30,
            'number': 65.0,
            'float': 62.4,
        }
        from petstore_api.model import format_test
        _response_for_200 = api_client.OpenApiResponse(
            content={
                self.json_content_type: api_client.MediaType(schema=format_test.FormatTest),
            },
        )
        response = self.__response(data)
        deserialized = _response_for_200.deserialize(response, self.configuration)
        self.assertTrue(isinstance(deserialized.body, format_test.FormatTest))

        with self.assertRaisesRegex(
            petstore_api.exceptions.ApiValueError,
            r"Invalid value `31`, value must be a multiple of `2` at \('args\[0\]', 'integer'\)"
        ):
            data = {
                'byte': '3',
                'date': '1970-01-01',
                'password': "abcdefghijkl",
                'integer': 31,  # Value is supposed to be multiple of '2'. An error must be raised
                'number': 65.0,
                'float': 62.4,
            }
            response = self.__response(data)
            _response_for_200.deserialize(response, self.configuration)

        # Disable JSON schema validation. No error should be raised during deserialization.
        configuration = petstore_api.Configuration()
        configuration.disabled_client_side_validations = "multipleOf"

        data = {
            'byte': '3',
            'date': '1970-01-01',
            'password': "abcdefghijkl",
            'integer': 31, # Value is supposed to be multiple of '2'
            'number': 65.0,
            'float': 62.4,
        }
        response = self.__response(data)
        deserialized = _response_for_200.deserialize(response, configuration)
        self.assertTrue(isinstance(deserialized.body, format_test.FormatTest))

        # Disable JSON schema validation but for a different keyword.
        # An error should be raised during deserialization.
        configuration = petstore_api.Configuration()
        configuration.disabled_client_side_validations = "maxItems"

        with self.assertRaisesRegex(
            petstore_api.exceptions.ApiValueError,
            r"Invalid value `31`, value must be a multiple of `2` at \('args\[0\]', 'integer'\)"
        ):
            data = {
                'byte': '3',
                'date': '1970-01-01',
                'password': "abcdefghijkl",
                'integer': 31, # Value is supposed to be multiple of '2'
                'number': 65.0,
                'float': 62.4,
            }
            response = self.__response(data)
            _response_for_200.deserialize(response, configuration)
