# # coding: utf-8
#
# # flake8: noqa
#
# """
# Run the tests.
# $ docker pull swaggerapi/petstore
# $ docker run -d -e SWAGGER_HOST=http://petstore.swagger.io -e SWAGGER_BASE_PATH=/v2 -p 80:8080 swaggerapi/petstore
# $ pip install nose (optional)
# $ cd petstore_api-python
# $ nosetests -v
# """
# from collections import namedtuple
# import json
# import re
# import unittest
#
# import petstore_api
# from petstore_api.model import cat, dog, isosceles_triangle, banana_req
# from petstore_api import Configuration, signing
#
# from petstore_api.schemas import (
#     file_type,
#     model_to_dict,
# )
#
# MockResponse = namedtuple('MockResponse', 'data')
#
# class DiscardUnknownPropertiesTests(unittest.TestCase):
#
#     def test_deserialize_banana_req_do_not_discard_unknown_properties(self):
#         """
#         deserialize bananaReq with unknown properties.
#         Strict validation is enabled.
#         Simple (non-composed) schema scenario.
#         """
#         config = Configuration(discard_unknown_keys=False)
#         api_client = petstore_api.ApiClient(config)
#         data = {
#             'lengthCm': 21.3,
#             'sweet': False,
#             # Below is an unknown property not explicitly declared in the OpenAPI document.
#             # It should not be in the payload because additional properties (undeclared) are
#             # not allowed in the bananaReq schema (additionalProperties: false).
#             'unknown_property': 'a-value'
#         }
#         response = MockResponse(data=json.dumps(data))
#
#         # Deserializing with strict validation raises an exception because the 'unknown_property'
#         # is undeclared.
#         with self.assertRaises(petstore_api.exceptions.ApiAttributeError) as cm:
#             deserialized = api_client.deserialize(response, ((banana_req.BananaReq),), True)
#         self.assertTrue(re.match("BananaReq has no attribute 'unknown_property' at.*", str(cm.exception)),
#             'Exception message: {0}'.format(str(cm.exception)))
#
#
#     def test_deserialize_isosceles_triangle_do_not_discard_unknown_properties(self):
#         """
#         deserialize IsoscelesTriangle with unknown properties.
#         Strict validation is enabled.
#         Composed schema scenario.
#         """
#         config = Configuration(discard_unknown_keys=False)
#         api_client = petstore_api.ApiClient(config)
#         data = {
#             'shape_type': 'Triangle',
#             'triangle_type': 'EquilateralTriangle',
#             # Below is an unknown property not explicitly declared in the OpenAPI document.
#             # It should not be in the payload because additional properties (undeclared) are
#             # not allowed in the schema (additionalProperties: false).
#             'unknown_property': 'a-value'
#         }
#         response = MockResponse(data=json.dumps(data))
#
#         # Deserializing with strict validation raises an exception because the 'unknown_property'
#         # is undeclared.
#         with self.assertRaises(petstore_api.ApiValueError) as cm:
#             deserialized = api_client.deserialize(response, ((isosceles_triangle.IsoscelesTriangle),), True)
#         self.assertTrue(re.match('.*Not all inputs were used.*unknown_property.*', str(cm.exception)),
#             'Exception message: {0}'.format(str(cm.exception)))
#
#
#     def test_deserialize_banana_req_discard_unknown_properties(self):
#         """
#         Deserialize bananaReq with unknown properties.
#         Discard unknown properties.
#         """
#         config = Configuration(discard_unknown_keys=True)
#         api_client = petstore_api.ApiClient(config)
#         data = {
#             'lengthCm': 21.3,
#             'sweet': False,
#             # Below are additional (undeclared) properties not specified in the bananaReq schema.
#             'unknown_property': 'a-value',
#             'more-unknown': [
#                 'a'
#             ]
#         }
#         # The 'unknown_property' is undeclared, which would normally raise an exception, but
#         # when discard_unknown_keys is set to True, the unknown properties are discarded.
#         response = MockResponse(data=json.dumps(data))
#         deserialized = api_client.deserialize(response, ((banana_req.BananaReq),), True)
#         self.assertTrue(isinstance(deserialized, banana_req.BananaReq))
#         # Check the 'unknown_property' and 'more-unknown' properties are not present in the
#         # output.
#         self.assertIn("length_cm", deserialized.to_dict().keys())
#         self.assertNotIn("unknown_property", deserialized.to_dict().keys())
#         self.assertNotIn("more-unknown", deserialized.to_dict().keys())
#
#     def test_deserialize_cat_do_not_discard_unknown_properties(self):
#         """
#         Deserialize Cat with unknown properties.
#         Strict validation is enabled.
#         """
#         config = Configuration(discard_unknown_keys=False)
#         api_client = petstore_api.ApiClient(config)
#         data = {
#             "class_name": "Cat",
#             "color": "black",
#             "declawed": True,
#             "dynamic-property": 12345,
#         }
#         response = MockResponse(data=json.dumps(data))
#
#         # Deserializing with strict validation does not raise an exception because the even though
#         # the 'dynamic-property' is undeclared, the 'Cat' schema defines the additionalProperties
#         # attribute.
#         deserialized = api_client.deserialize(response, ((cat.Cat),), True)
#         self.assertTrue(isinstance(deserialized, cat.Cat))
#         self.assertIn('color', deserialized.to_dict())
#         self.assertEqual(deserialized['color'], 'black')
#
#     def test_deserialize_cat_discard_unknown_properties(self):
#         """
#         Deserialize Cat with unknown properties.
#         Request to discard unknown properties, but Cat is composed schema
#         with one inner schema that has 'additionalProperties' set to true.
#         """
#         config = Configuration(discard_unknown_keys=True)
#         api_client = petstore_api.ApiClient(config)
#         data = {
#             "class_name": "Cat",
#             "color": "black",
#             "declawed": True,
#             # Below are additional (undeclared) properties.
#             "my_additional_property": 123,
#         }
#         # The 'my_additional_property' is undeclared, but 'Cat' has a 'Address' type through
#         # the allOf: [ $ref: '#/components/schemas/Address' ].
#         response = MockResponse(data=json.dumps(data))
#         deserialized = api_client.deserialize(response, ((cat.Cat),), True)
#         self.assertTrue(isinstance(deserialized, cat.Cat))
#         # Check the 'unknown_property' and 'more-unknown' properties are not present in the
#         # output.
#         self.assertIn("declawed", deserialized.to_dict().keys())
#         self.assertIn("my_additional_property", deserialized.to_dict().keys())
#
