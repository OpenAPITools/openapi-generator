# coding: utf-8

# flake8: noqa


import os
import time
import unittest

from pydantic import ValidationError

import petstore_api


class ModelTests(unittest.TestCase):

    def setUp(self):
        self.pet = petstore_api.Pet(name="test name", photoUrls=["string"])
        self.pet.id = 1
        self.pet.status = "available"
        cate = petstore_api.Category(name="dog")
        cate.id = 1
        cate.name = "dog"
        self.pet.category = cate
        tag = petstore_api.Tag()
        tag.id = 1
        self.pet.tags = [tag]

    def test_cat(self):
        self.cat = petstore_api.Cat(className="cat")
        self.assertEqual("cat", self.cat.class_name)
        self.assertEqual("red", self.cat.color)
        cat_str = "{'className': 'cat', 'color': 'red', 'declawed': None}"
        self.assertEqual(cat_str, self.cat.to_str())

    def test_to_str(self):
        data = ("{'category': {'id': 1, 'name': 'dog'},\n"
                " 'id': 1,\n"
                " 'name': 'test name',\n"
                " 'photoUrls': ['string'],\n"
                " 'status': 'available',\n"
                " 'tags': [{'id': 1, 'name': None}]}")
        self.assertEqual(data, self.pet.to_str())

    def test_equal(self):
        self.pet1 = petstore_api.Pet(name="test name", photoUrls=["string"])
        self.pet1.id = 1
        self.pet1.status = "available"
        cate1 = petstore_api.Category(name="dog")
        cate1.id = 1
        # cate1.name = "dog"
        self.pet.category = cate1
        tag1 = petstore_api.Tag()
        tag1.id = 1
        self.pet1.tags = [tag1]

        self.pet2 = petstore_api.Pet(name="test name", photoUrls=["string"])
        self.pet2.id = 1
        self.pet2.status = "available"
        cate2 = petstore_api.Category(name="dog")
        cate2.id = 1
        cate2.name = "dog"
        self.pet.category = cate2
        tag2 = petstore_api.Tag()
        tag2.id = 1
        self.pet2.tags = [tag2]

        self.assertTrue(self.pet1 == self.pet2)

        # reset pet1 tags to empty array so that object comparison returns false
        self.pet1.tags = []
        self.assertFalse(self.pet1 == self.pet2)

    def test_oneOf(self):
        # test new Pig
        new_pig = petstore_api.Pig()
        self.assertEqual("null", new_pig.to_json())
        self.assertEqual(None, new_pig.actual_instance)

        # test from_json
        json_str = '{"className": "BasquePig", "color": "red"}'
        p = petstore_api.Pig.from_json(json_str)
        self.assertIsInstance(p.actual_instance, petstore_api.BasquePig)

        # test init
        basque_pig = p.actual_instance
        pig2 = petstore_api.Pig(actual_instance=basque_pig)
        self.assertIsInstance(pig2.actual_instance, petstore_api.BasquePig)

        # test failed init
        try:
            pig3 = petstore_api.Pig(actual_instance="123")
            self.assertTrue(False)  # this line shouldn't execute
        except ValueError as e:
            self.assertIn("or instance of BasquePig", str(e))
            self.assertIn("or instance of DanishPig", str(e))

        # failure
        try:
            p2 = petstore_api.Pig.from_json("1")
            self.assertTrue(False)  # this line shouldn't execute
        except ValueError as e:
            #   No match found when deserializing the JSON string into Pig with oneOf schemas: BasquePig, DanishPig. Details: 1 validation error for BasquePig
            #     Input should be a valid dictionary or instance of BasquePig [type=model_type, input_value=1, input_type=int]
            #       For further information visit https://errors.pydantic.dev/2.3/v/model_type, 1 validation error for DanishPig
            #     Input should be a valid dictionary or instance of DanishPig [type=model_type, input_value=1, input_type=int]
            #       For further information visit https://errors.pydantic.dev/2.3/v/model_type
            self.assertIn("No match found when deserializing the JSON string into Pig with oneOf schemas: BasquePig, DanishPig.", str(e))
            self.assertIn("Input should be a valid dictionary or instance of BasquePig", str(e))
            self.assertIn("Input should be a valid dictionary or instance of DanishPig", str(e))

        # test to_json
        self.assertEqual(p.to_json(), '{"className": "BasquePig", "color": "red"}')

        # test nested property
        nested = petstore_api.WithNestedOneOf(size = 1, nested_pig = p)
        self.assertEqual(nested.to_json(), '{"size": 1, "nested_pig": {"className": "BasquePig", "color": "red"}}')

        nested_json = nested.to_json()
        nested2 = petstore_api.WithNestedOneOf.from_json(nested_json)
        self.assertEqual(nested2.to_json(), nested_json)

    def test_anyOf(self):
        # test new AnyOfPig
        new_anypig = petstore_api.AnyOfPig()
        self.assertEqual("null", new_anypig.to_json())
        self.assertEqual(None, new_anypig.actual_instance)

        # test from_json
        json_str = '{"className": "BasquePig", "color": "red"}'
        p = petstore_api.AnyOfPig.from_json(json_str)
        self.assertIsInstance(p.actual_instance, petstore_api.BasquePig)

        # test init
        basque_pig = p.actual_instance
        pig2 = petstore_api.Pig(actual_instance=basque_pig)
        self.assertIsInstance(pig2.actual_instance, petstore_api.BasquePig)

        # test failed init
        try:
            pig3 = petstore_api.AnyOfPig(actual_instance="123")
            self.assertTrue(False)  # this line shouldn't execute
        except ValueError as e:
            #   pydantic_core._pydantic_core.ValidationError: 1 validation error for AnyOfPig
            #   actual_instance
            #     Value error, No match found when setting the actual_instance in AnyOfPig with anyOf schemas: BasquePig, DanishPig. Details: Error! Input type `<class 'str'>` is not `BasquePig`, Error! Input type `<class 'str'>` is not `DanishPig` [type=value_error, input_value='123', input_type=str]
            #       For further information visit https://errors.pydantic.dev/2.4/v/value_error
            self.assertIn("No match found when setting the actual_instance in AnyOfPig with anyOf schemas: BasquePig, DanishPig.", str(e))
            self.assertIn("Input type `<class 'str'>` is not `BasquePig`", str(e))
            self.assertIn("Input type `<class 'str'>` is not `DanishPig`", str(e))

        # failure
        try:
            p2 = petstore_api.AnyOfPig.from_json("1")
            self.assertTrue(False)  # this line shouldn't execute
        except ValueError as e:
            # No match found when deserializing the JSON string into AnyOfPig with anyOf schemas: BasquePig, DanishPig. Details: 1 validation error for BasquePig
            #   Input should be a valid dictionary or instance of BasquePig [type=model_type, input_value=1, input_type=int]
            #     For further information visit https://errors.pydantic.dev/2.3/v/model_type, 1 validation error for DanishPig
            #   Input should be a valid dictionary or instance of DanishPig [type=model_type, input_value=1, input_type=int]
            #     For further information visit https://errors.pydantic.dev/2.3/v/model_type
            self.assertIn( "No match found when deserializing the JSON string into AnyOfPig with anyOf schemas: BasquePig, DanishPig", str(e))
            self.assertIn("Input should be a valid dictionary or instance of BasquePig", str(e))
            self.assertIn("Input should be a valid dictionary or instance of DanishPig", str(e))

        # test to_json
        self.assertEqual(p.to_json(), '{"className": "BasquePig", "color": "red"}')

    def test_inheritance(self):
        dog = petstore_api.Dog(breed="bulldog", className="dog", color="white")
        self.assertEqual(dog.to_json(), '{"className": "dog", "color": "white", "breed": "bulldog"}')
        self.assertEqual(dog.to_dict(), {'breed': 'bulldog', 'className':
            'dog', 'color': 'white'})
        dog2 = petstore_api.Dog.from_json(dog.to_json())
        self.assertEqual(dog2.breed, 'bulldog')
        self.assertEqual(dog2.class_name, "dog")
        self.assertEqual(dog2.color, 'white')

    @unittest.skip("TODO: pydantic v2: Optional[StrictStr] is not strict like StrictStr")
    def test_list(self):
        # should throw exception as var_123_list should be string
        try:
            l3 = petstore_api.List(var_123_list=123)
            self.assertTrue(False)  # this line shouldn't execute
        except ValueError as e:
            #error_message = (
            #    "1 validation error for List\n"
            #    "123-list\n"
            #    "  str type expected (type=type_error.str)\n")
            self.assertTrue("str type expected" in str(e))

        l = petstore_api.List(var_123_list="bulldog")
        self.assertEqual(l.to_json(), '{"123-list":"bulldog"}')
        self.assertEqual(l.to_dict(), {'123-list': 'bulldog'})
        l2 = petstore_api.List.from_json(l.to_json())
        self.assertEqual(l2.var_123_list, 'bulldog')

        self.assertTrue(isinstance(l2, petstore_api.List))

    def test_enum_ref_property(self):
        # test enum ref property
        # test to_json
        d = petstore_api.OuterObjectWithEnumProperty(value=petstore_api.OuterEnumInteger.NUMBER_1)
        self.assertEqual(d.to_json(), '{"value": 1}')
        d2 = petstore_api.OuterObjectWithEnumProperty(value=petstore_api.OuterEnumInteger.NUMBER_1, str_value=petstore_api.OuterEnum.DELIVERED)
        self.assertEqual(d2.to_json(), '{"str_value": "delivered", "value": 1}')
        # test from_json (round trip)
        d3 = petstore_api.OuterObjectWithEnumProperty.from_json(d2.to_json())
        self.assertEqual(d3.str_value, petstore_api.OuterEnum.DELIVERED)
        self.assertEqual(d3.value, petstore_api.OuterEnumInteger.NUMBER_1)
        self.assertEqual(d3.to_json(), '{"str_value": "delivered", "value": 1}')

    @unittest.skip("TODO: pydantic v2: 'float' field alias the 'float' type used by 'number'")
    def test_float_strict_type(self):
        # assigning 123 to float shouldn't throw an exception
        a = petstore_api.FormatTest(number=39.8, float=123, byte=bytes("string", 'utf-8'), date="2013-09-17", password="testing09876")
        self.assertEqual(a.float, 123.0)

        json_str = "{\"number\": 34.5, \"float\": \"456\", \"date\": \"2013-12-08\", \"password\": \"empty1234567\", \"pattern_with_digits\": \"1234567890\", \"pattern_with_digits_and_delimiter\": \"image_123\", \"string_with_double_quote_pattern\": \"this is \\\"something\\\"\", \"string\": \"string\"}"
        # no exception thrown when assigning 456 (integer) to float type since strict is set to false
        f = petstore_api.FormatTest.from_json(json_str)
        self.assertEqual(f.float, 456.0)

    def test_valdiator(self):
        # test regular expression
        a = petstore_api.FormatTest(number=123.45, byte=bytes("string", 'utf-8'), date="2013-09-17", password="testing09876")
        try:
            a.pattern_with_digits_and_delimiter = "123"
            self.assertTrue(False) # this line shouldn't execute
        except ValueError as e:
            self.assertTrue(r"must validate the regular expression /^image_\d{1,3}$/i" in str(e))

        a.pattern_with_digits_and_delimiter = "IMAGE_123"
        self.assertEqual(a.pattern_with_digits_and_delimiter, "IMAGE_123")
        a.pattern_with_digits_and_delimiter = "image_123"
        self.assertEqual(a.pattern_with_digits_and_delimiter, "image_123")

    def test_inline_enum_validator(self):
        self.pet = petstore_api.Pet(name="test name", photoUrls=["string"])
        self.pet.id = 1
        try:
            self.pet.status = "error"
            self.assertTrue(False) # this line shouldn't execute
        except ValueError as e:
            self.assertTrue("must be one of enum values ('available', 'pending', 'sold')" in str(e))

    def test_constraints(self):
        rgb = [128, 128, 128]
        rgba = [128, 128, 128, 128]
        hex_color = "#00FF00"

        # These should all pass
        color = petstore_api.Color(oneof_schema_1_validator=rgb)
        self.assertEqual(rgb, color.oneof_schema_1_validator)

        color = petstore_api.Color(oneof_schema_2_validator=rgba)
        self.assertEqual(rgba, color.oneof_schema_2_validator)

        color = petstore_api.Color(oneof_schema_3_validator=hex_color)
        self.assertEqual(hex_color, color.oneof_schema_3_validator)

        try:
            petstore_api.Color(oneof_schema_1_validator=rgba)
            self.fail("invalid validation")
        except ValidationError as e:
            self.assertIn("List should have at most 3 items after validation, not 4", str(e))

        try:
            petstore_api.Color(oneof_schema_2_validator=rgb)
            self.fail("invalid validation")
        except ValidationError as e:
            self.assertIn("List should have at least 4 items after validation, not 3", str(e))

        try:
            petstore_api.Color(oneof_schema_3_validator="too long string")
            self.fail("invalid validation")
        except ValidationError as e:
            self.assertIn("String should have at most 7 characters", str(e))
