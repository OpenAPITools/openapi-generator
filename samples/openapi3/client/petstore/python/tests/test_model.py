# coding: utf-8

# flake8: noqa

import json
import os
import time
import unittest

import petstore_api


class ModelTests(unittest.TestCase):

    def setUp(self):
        self.pet = petstore_api.Pet(name="test name", photo_urls=["string"])
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
        self.cat = petstore_api.Cat(class_name="cat")
        self.assertEqual("cat", self.cat.class_name)
        self.assertEqual("red", self.cat.color)
        cat_str = ("{'additional_properties': {},\n"
                  " 'className': 'cat',\n"
                  " 'color': 'red',\n"
                  " 'declawed': None}")
        self.assertEqual(cat_str, self.cat.to_str())

    def test_to_str(self):
        data = ("{'additional_properties': {},\n"
                " 'category': {'additional_properties': {}, 'id': 1, 'name': 'dog'},\n"
                " 'id': 1,\n"
                " 'name': 'test name',\n"
                " 'photoUrls': ['string'],\n"
                " 'status': 'available',\n"
                " 'tags': [{'additional_properties': {}, 'id': 1, 'name': None}]}")
        self.assertEqual(data, self.pet.to_str())

    def test_equal(self):
        self.pet1 = petstore_api.Pet(name="test name", photo_urls=["string"])
        self.pet1.id = 1
        self.pet1.status = "available"
        cate1 = petstore_api.Category(name="dog")
        cate1.id = 1
        # cate1.name = "dog"
        self.pet.category = cate1
        tag1 = petstore_api.Tag()
        tag1.id = 1
        self.pet1.tags = [tag1]

        self.pet2 = petstore_api.Pet(name="test name", photo_urls=["string"])
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

    def test_oneOf_array_of_integers(self):
        # test new Color 
        new_color = petstore_api.Color()
        self.assertEqual("null", new_color.to_json())
        self.assertEqual(None, new_color.actual_instance)

        # test the oneof schema validator
        json_str = '[12,34,56]'
        array_of_integers = json.loads(json_str)
        # no error should be thrown
        new_color.oneof_schema_1_validator = array_of_integers
        new_color.actual_instance = array_of_integers
        new_color.actual_instance = None

        # test the oneof schema validator with invalid input 
        json_str = '[12,34,56120938]'
        array_of_integers = json.loads(json_str)
        try:
            new_color.oneof_schema_1_validator = array_of_integers
        except ValueError as e:
            self.assertTrue("ensure this value is less than or equal to 255" in str(e))

        try:
            new_color.actual_instance = array_of_integers
        except ValueError as e:
            self.assertTrue("ensure this value is less than or equal to 255" in str(e))

        # test from_josn
        json_str = '[12,34,56]'
        p = petstore_api.Color.from_json(json_str)
        self.assertEqual(p.actual_instance, [12, 34, 56])

        try:
            p = petstore_api.Color.from_json('[2342112,0,0,0]')
        except ValueError as e:
            self.assertTrue("ensure this value is less than or equal to 255" in str(e))

        # test to_json, to_dict method
        json_str = '[12,34,56]'
        p = petstore_api.Color.from_json(json_str)
        self.assertEqual(p.to_json(), "[12, 34, 56]")
        self.assertEqual(p.to_dict(), [12, 34, 56])

        # test nullable
        p = petstore_api.Color.from_json(None)
        self.assertEqual(p.actual_instance, None)

    def test_oneof_enum_string(self):
        enum_string1 = petstore_api.EnumString1('a')
        # test from_json
        oneof_enum = petstore_api.OneOfEnumString.from_json('"a"')
        # test from_dict
        oneof_enum = petstore_api.OneOfEnumString.from_dict("a")
        nested = petstore_api.WithNestedOneOf(size = 1, nested_oneof_enum_string = oneof_enum)
        # test to_json
        self.assertEqual(nested.to_json(), '{"size": 1, "nested_oneof_enum_string": "a"}')
        # test from_json
        nested = petstore_api.WithNestedOneOf.from_json('{"size": 1, "nested_oneof_enum_string": "c"}')
        self.assertEqual(nested.to_json(), '{"size": 1, "nested_oneof_enum_string": "c"}')
        # test from_dict
        nested = petstore_api.WithNestedOneOf.from_dict({"size": 1, "nested_oneof_enum_string": "c"})
        # test to_dict
        self.assertEqual(nested.to_dict(), {"size": 1, "nested_oneof_enum_string": "c"})
        # invalid enum value
        try:
            nested2 = petstore_api.WithNestedOneOf.from_json('{"size": 1, "nested_oneof_enum_string": "e"}')
        except ValueError as e:
            self.assertTrue("'e' is not a valid EnumString1, 'e' is not a valid EnumString" in str(e))

        # test the constructor
        enum_string1 = petstore_api.EnumString1('a')
        constructor1 = petstore_api.OneOfEnumString(actual_instance=enum_string1)
        self.assertEqual(constructor1.actual_instance, enum_string1)
        constructor2 = petstore_api.OneOfEnumString(enum_string1)
        self.assertEqual(constructor2.actual_instance, enum_string1)
        constructor3 = petstore_api.OneOfEnumString()
        self.assertEqual(constructor3.actual_instance, None)

    def test_anyOf_array_of_integers(self):
        # test new Color 
        new_color = petstore_api.AnyOfColor()
        self.assertEqual("null", new_color.to_json())
        self.assertEqual(None, new_color.actual_instance)

        # test the oneof schema validator
        json_str = '[12,34,56]'
        array_of_integers = json.loads(json_str)
        # no error should be thrown
        new_color.anyof_schema_1_validator = array_of_integers
        new_color.actual_instance = array_of_integers

        # test the oneof schema validator with invalid input 
        json_str = '[12,34,56120938]'
        array_of_integers = json.loads(json_str)
        try:
            new_color.anyof_schema_1_validator = array_of_integers
        except ValueError as e:
            self.assertTrue("ensure this value is less than or equal to 255" in str(e))

        try:
            new_color.actual_instance = array_of_integers
        except ValueError as e:
            self.assertTrue("ensure this value is less than or equal to 255" in str(e))

        # test from_josn
        json_str = '[12,34,56]'
        p = petstore_api.AnyOfColor.from_json(json_str)
        self.assertEqual(p.actual_instance, [12, 34,56])

        try:
            p = petstore_api.AnyOfColor.from_json('[2342112,0,0,0]')
        except ValueError as e:
            self.assertTrue("ensure this value is less than or equal to 255" in str(e))

    def test_oneOf(self):
        # test new Pig
        bp = petstore_api.BasquePig.from_dict({"className": "BasquePig", "color": "red"})
        new_pig = petstore_api.Pig()
        self.assertEqual("null", new_pig.to_json())
        self.assertEqual(None, new_pig.actual_instance)
        new_pig2 = petstore_api.Pig(actual_instance=bp)
        self.assertEqual('{"className": "BasquePig", "color": "red"}', new_pig2.to_json())
        new_pig3 = petstore_api.Pig(bp)
        self.assertEqual('{"className": "BasquePig", "color": "red"}', new_pig3.to_json())
        try:
            new_pig4 = petstore_api.Pig(bp, actual_instance=bp)
        except ValueError as e:
            self.assertTrue("If position argument is used, keyword argument cannot be used.", str(e))
        try:
            new_pig5 = petstore_api.Pig(bp, bp)
        except ValueError as e:
            self.assertTrue("If position argument is used, only 1 is allowed to set `actual_instance`", str(e))

        # test from_json
        json_str = '{"className": "BasquePig", "color": "red"}'
        p = petstore_api.Pig.from_json(json_str)
        self.assertIsInstance(p.actual_instance, petstore_api.BasquePig)

        # test from_dict
        json_dict = {"className": "BasquePig", "color": "red"}
        p = petstore_api.Pig.from_dict(json_dict)
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
            self.assertTrue("No match found when setting `actual_instance` in Pig with oneOf schemas: BasquePig, DanishPig" in str(e))

        # failure
        try:
            p2 = petstore_api.Pig.from_json("1")
            self.assertTrue(False)  # this line shouldn't execute
        except AttributeError as e:
            self.assertEqual(str(e), "'int' object has no attribute 'get'")
        # comment out below as the error message is different using oneOf discriminator lookup option
        #except ValueError as e:
        #    error_message = (
        #        "No match found when deserializing the JSON string into Pig with oneOf schemas: BasquePig, DanishPig. "
        #        "Details: 1 validation error for BasquePig\n"
        #        "__root__\n"
        #        "  BasquePig expected dict not int (type=type_error), 1 validation error for DanishPig\n"
        #        "__root__\n"
        #        "  DanishPig expected dict not int (type=type_error)")
        #    self.assertEqual(str(e), error_message)

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

        # test from_dict
        json_dict = {"className": "BasquePig", "color": "red"}
        p = petstore_api.AnyOfPig.from_dict(json_dict)
        self.assertIsInstance(p.actual_instance, petstore_api.BasquePig)

        # test init
        basque_pig = p.actual_instance
        pig2 = petstore_api.AnyOfPig(actual_instance=basque_pig)
        self.assertIsInstance(pig2.actual_instance, petstore_api.BasquePig)

        # test failed init
        try:
            pig3 = petstore_api.AnyOfPig(actual_instance="123")
            self.assertTrue(False)  # this line shouldn't execute
        except ValueError as e:
            self.assertTrue(
                "No match found when setting the actual_instance in AnyOfPig with anyOf schemas: BasquePig, "
                "DanishPig" in str(e))

        # failure
        try:
            p2 = petstore_api.AnyOfPig.from_json("1")
            self.assertTrue(False)  # this line shouldn't execute
        except ValueError as e:
            error_message = (
                "No match found when deserializing the JSON string into AnyOfPig with anyOf schemas: BasquePig, "
                "DanishPig. Details: 1 validation error for BasquePig\n"
                "__root__\n"
                "  BasquePig expected dict not int (type=type_error), 1 validation error for DanishPig\n"
                "__root__\n"
                "  DanishPig expected dict not int (type=type_error)")
            self.assertEqual(str(e), error_message)

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
        self.assertEqual(l.to_json(), '{"123-list": "bulldog"}')
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
        d4 = petstore_api.OuterObjectWithEnumProperty(value=petstore_api.OuterEnumInteger.NUMBER_1, str_value=None)
        self.assertEqual(d4.to_json(), '{"value": 1, "str_value": null}')
        d5 = petstore_api.OuterObjectWithEnumProperty(value=petstore_api.OuterEnumInteger.NUMBER_1)
        self.assertEqual(d5.__fields_set__, {'value'})
        d5.str_value = None # set None explicitly
        self.assertEqual(d5.__fields_set__, {'value', 'str_value'})
        self.assertEqual(d5.to_json(), '{"value": 1, "str_value": null}')

    def test_valdiator(self):
        # test regular expression
        a = petstore_api.FormatTest(number=123.45, byte=bytes("string", 'utf-8'), date="2013-09-17", password="testing09876")
        try:
            a.pattern_with_digits_and_delimiter = "123"
            self.assertTrue(False) # this line shouldn't execute
        except ValueError as e:
            self.assertTrue(r"must validate the regular expression /^image_\d{1,3}$/i" in str(e))

        # test None with optional string (with regualr expression)
        a = petstore_api.FormatTest(number=123.45, byte=bytes("string", 'utf-8'), date="2013-09-17", password="testing09876")
        a.string = None # shouldn't throw an exception

        a.pattern_with_digits_and_delimiter = "IMAGE_123"
        self.assertEqual(a.pattern_with_digits_and_delimiter, "IMAGE_123")
        a.pattern_with_digits_and_delimiter = "image_123"
        self.assertEqual(a.pattern_with_digits_and_delimiter, "image_123")

    def test_inline_enum_validator(self):
        self.pet = petstore_api.Pet(name="test name", photo_urls=["string"])
        self.pet.id = 1
        try:
            self.pet.status = "error"
            self.assertTrue(False) # this line shouldn't execute
        except ValueError as e:
            self.assertTrue("must be one of enum values ('available', 'pending', 'sold')" in str(e))

    def test_object_id(self):
        pet_ap = petstore_api.Pet(name="test name", photo_urls=["string"])
        pet_ap2 = petstore_api.Pet(name="test name", photo_urls=["string"])
        self.assertNotEqual(id(pet_ap), id(pet_ap2))

        pet_ap3 = petstore_api.Pet.from_dict(pet_ap.to_dict())
        pet_ap4 = petstore_api.Pet.from_dict(pet_ap.to_dict())
        self.assertNotEqual(id(pet_ap3), id(pet_ap4))


    def test_additional_properties(self):
        pet_ap = petstore_api.Pet(name="test name", photo_urls=["string"])
        pet_ap.id = 1
        pet_ap.status = "available"

        pet_ap2 = petstore_api.Pet(name="test name", photo_urls=["string"])
        pet_ap2.id = 1
        pet_ap2.status = "available"

        self.assertNotEqual(id(pet_ap.additional_properties), id(pet_ap2.additional_properties))

        pet_ap.additional_properties["something-new"] = "haha"
        self.assertEqual(pet_ap.to_json(), '{"id": 1, "name": "test name", "photoUrls": ["string"], "status": "available", "something-new": "haha"}')
        self.assertEqual(type(pet_ap2.additional_properties), dict)
        self.assertNotEqual(id(pet_ap.additional_properties), id(pet_ap2.additional_properties))
        self.assertEqual(pet_ap.additional_properties["something-new"], "haha")

        try:
            _tmp = pet_ap2.additional_properties["something-new"]
            self.assertTrue(False) # this line shouldn't execute
        except KeyError as e:
            self.assertEqual("'something-new'", str(e))

        pet_ap_dict = pet_ap.to_dict()
        pet_ap_dict["something-new"] = 123
        pet_ap_dict["array"] = ["a", "b"]
        pet_ap_dict["dict"] = {"key999": "value999"}

        pet_ap2 = petstore_api.Pet.from_dict(pet_ap_dict)

        self.assertEqual(pet_ap2.additional_properties["array"], ["a", "b"])
        self.assertEqual(pet_ap2.additional_properties["something-new"], 123)
        self.assertEqual(pet_ap2.additional_properties["dict"], {"key999": "value999"})

    def test_nullable(self):
        h = petstore_api.HealthCheckResult(nullable_message="Not none")
        self.assertEqual(h.to_json(), '{"NullableMessage": "Not none"}')

        h.nullable_message = None
        self.assertEqual(h.to_json(), '{"NullableMessage": null}')

        #import json
        #dictionary ={ 
        #  "id": "04", 
        #  "name": "sunil", 
        #  "department": None
        #} 
        #      
        ## Serializing json  
        #json_object = json.dumps(dictionary) 
        #self.assertEqual(json_object, "")

    def test_inline_enum_default(self):
        enum_test = petstore_api.EnumTest(enum_string_required="lower")
        self.assertEqual(enum_test.enum_integer_default, 5)

    def test_object_with_optional_dict(self):
        # for https://github.com/OpenAPITools/openapi-generator/issues/14913
        # shouldn't throw exception by the optional dict property
        a = petstore_api.ParentWithOptionalDict.from_dict({})
        self.assertFalse(a is None)

        b = petstore_api.ParentWithOptionalDict.from_dict({"optionalDict": {"key": {"aProperty": {"a": "b"}}}})
        self.assertFalse(b is None)
        self.assertEqual(b.optional_dict["key"].a_property["a"], "b")

    def test_object_with_dict_of_dict_of_object(self):
        # for https://github.com/OpenAPITools/openapi-generator/issues/15135
        d = {"optionalDict": {"a": {"b": {"aProperty": "value"}}}}
        a = petstore_api.Parent.from_dict(d)
        self.assertEqual(a.to_dict(), d)

    def test_eum_class(self):
        a = petstore_api.EnumClass("-efg")
        self.assertEqual(a.value, "-efg")
        self.assertEqual(a.name, "MINUS_EFG")
        self.assertEqual(a, "-efg")

    def test_nullable_property_pattern(self):
        a = petstore_api.NullableProperty(id=12, name=None)
        self.assertEqual(a.id, 12)
        self.assertEqual(a.name, None)

    def test_int_or_string_oneof(self):
        a = petstore_api.IntOrString("-efg")
        self.assertEqual(a.actual_instance, "-efg")
        a = petstore_api.IntOrString(100)
        self.assertEqual(a.actual_instance, 100)

        try:
            a = petstore_api.IntOrString(1)
        except ValueError as e:
            self.assertTrue("ensure this value is greater than or equal to 10" in str(e))

    def test_map_of_array_of_model(self):
        a = petstore_api.MapOfArrayOfModel()
        t = petstore_api.Tag(id=123, name="tag name")
        a.shop_id_to_org_online_lip_map = {"somekey": [t]}
        self.assertEqual(a.to_dict(), {'shopIdToOrgOnlineLipMap': {'somekey': [{'id': 123, 'name': 'tag name'}]}})
        self.assertEqual(a.to_json(), '{"shopIdToOrgOnlineLipMap": {"somekey": [{"id": 123, "name": "tag name"}]}}')
        a2 = petstore_api.MapOfArrayOfModel.from_dict(a.to_dict())
        self.assertEqual(a.to_dict(), a2.to_dict())

    def test_array_of_array_of_model(self):
        a = petstore_api.ArrayOfArrayOfModel()
        t = petstore_api.Tag(id=123, name="tag name")
        a.another_property = [[t]]
        self.assertEqual(a.to_dict(), {'another_property': [[ {'id': 123, 'name': 'tag name'} ]]})
        self.assertEqual(a.to_json(), '{"another_property": [[{"id": 123, "name": "tag name"}]]}')
        a2 = petstore_api.ArrayOfArrayOfModel.from_dict(a.to_dict())
        self.assertEqual(a.to_dict(), a2.to_dict())

    def test_object_with_additional_properties(self):
        a = petstore_api.ObjectToTestAdditionalProperties()
        a.additional_properties = { "abc": 123 }
        # should not throw the following errors:
        #   pydantic.errors.ConfigError: field "additional_properties" not yet prepared so type is still a ForwardRef, you might need to call ObjectToTestAdditionalProperties.update_forward_refs().

    def test_first_ref(self):
        # shouldn't throw "still a ForwardRef" error
        a = petstore_api.FirstRef.from_dict({})
        self.assertEqual(a.to_json(), "{}")

    def test_allof(self):
        # for issue 16104
        model = petstore_api.Tiger.from_json('{"skill": "none", "type": "tiger", "info": {"name": "creature info"}}')
        # shouldn't throw NameError
        self.assertEqual(model.to_json(), '{"skill": "none", "type": "tiger", "info": {"name": "creature info"}}')

    def test_additional_properties(self):
        a1 = petstore_api.AdditionalPropertiesAnyType()
        a1.additional_properties = { "abc": 123 }
        self.assertEqual(a1.to_dict(), {"abc": 123})
        self.assertEqual(a1.to_json(), "{\"abc\": 123}")

        a2 = petstore_api.AdditionalPropertiesObject()
        a2.additional_properties = { "efg": 45.6 }
        self.assertEqual(a2.to_dict(), {"efg": 45.6})
        self.assertEqual(a2.to_json(), "{\"efg\": 45.6}")

        a3 = petstore_api.AdditionalPropertiesWithDescriptionOnly()
        a3.additional_properties = { "xyz": 45.6 }
        self.assertEqual(a3.to_dict(), {"xyz": 45.6})
        self.assertEqual(a3.to_json(), "{\"xyz\": 45.6}")
