# coding: utf-8

# flake8: noqa

# import all models into this package
# if you have many models here with many references from one model to another this may
# raise a RecursionError
# to avoid this, import only the models that you directly need like:
# from from petstore_api.model.pet import Pet
# or import this package, but before doing it, use:
# import sys
# sys.setrecursionlimit(n)

from petstore_api.model.additional_properties_class import AdditionalPropertiesClass
from petstore_api.model.additional_properties_with_array_of_enums import AdditionalPropertiesWithArrayOfEnums
from petstore_api.model.address import Address
from petstore_api.model.animal import Animal
from petstore_api.model.animal_farm import AnimalFarm
from petstore_api.model.any_type_not_string import AnyTypeNotString
from petstore_api.model.api_response import ApiResponse
from petstore_api.model.apple import Apple
from petstore_api.model.apple_req import AppleReq
from petstore_api.model.array_holding_any_type import ArrayHoldingAnyType
from petstore_api.model.array_of_array_of_number_only import ArrayOfArrayOfNumberOnly
from petstore_api.model.array_of_enums import ArrayOfEnums
from petstore_api.model.array_of_number_only import ArrayOfNumberOnly
from petstore_api.model.array_test import ArrayTest
from petstore_api.model.array_with_validations_in_items import ArrayWithValidationsInItems
from petstore_api.model.banana import Banana
from petstore_api.model.banana_req import BananaReq
from petstore_api.model.bar import Bar
from petstore_api.model.basque_pig import BasquePig
from petstore_api.model.boolean import Boolean
from petstore_api.model.boolean_enum import BooleanEnum
from petstore_api.model.capitalization import Capitalization
from petstore_api.model.cat import Cat
from petstore_api.model.category import Category
from petstore_api.model.child_cat import ChildCat
from petstore_api.model.class_model import ClassModel
from petstore_api.model.client import Client
from petstore_api.model.complex_quadrilateral import ComplexQuadrilateral
from petstore_api.model.composed_any_of_different_types_no_validations import ComposedAnyOfDifferentTypesNoValidations
from petstore_api.model.composed_array import ComposedArray
from petstore_api.model.composed_bool import ComposedBool
from petstore_api.model.composed_none import ComposedNone
from petstore_api.model.composed_number import ComposedNumber
from petstore_api.model.composed_object import ComposedObject
from petstore_api.model.composed_one_of_different_types import ComposedOneOfDifferentTypes
from petstore_api.model.composed_string import ComposedString
from petstore_api.model.currency import Currency
from petstore_api.model.danish_pig import DanishPig
from petstore_api.model.date_time_test import DateTimeTest
from petstore_api.model.date_time_with_validations import DateTimeWithValidations
from petstore_api.model.date_with_validations import DateWithValidations
from petstore_api.model.decimal_payload import DecimalPayload
from petstore_api.model.dog import Dog
from petstore_api.model.drawing import Drawing
from petstore_api.model.enum_arrays import EnumArrays
from petstore_api.model.enum_class import EnumClass
from petstore_api.model.enum_test import EnumTest
from petstore_api.model.equilateral_triangle import EquilateralTriangle
from petstore_api.model.file import File
from petstore_api.model.file_schema_test_class import FileSchemaTestClass
from petstore_api.model.foo import Foo
from petstore_api.model.format_test import FormatTest
from petstore_api.model.fruit import Fruit
from petstore_api.model.fruit_req import FruitReq
from petstore_api.model.gm_fruit import GmFruit
from petstore_api.model.grandparent_animal import GrandparentAnimal
from petstore_api.model.has_only_read_only import HasOnlyReadOnly
from petstore_api.model.health_check_result import HealthCheckResult
from petstore_api.model.integer_enum import IntegerEnum
from petstore_api.model.integer_enum_big import IntegerEnumBig
from petstore_api.model.integer_enum_one_value import IntegerEnumOneValue
from petstore_api.model.integer_enum_with_default_value import IntegerEnumWithDefaultValue
from petstore_api.model.integer_max10 import IntegerMax10
from petstore_api.model.integer_min15 import IntegerMin15
from petstore_api.model.isosceles_triangle import IsoscelesTriangle
from petstore_api.model.mammal import Mammal
from petstore_api.model.map_test import MapTest
from petstore_api.model.mixed_properties_and_additional_properties_class import MixedPropertiesAndAdditionalPropertiesClass
from petstore_api.model.model200_response import Model200Response
from petstore_api.model.model_return import ModelReturn
from petstore_api.model.money import Money
from petstore_api.model.name import Name
from petstore_api.model.no_additional_properties import NoAdditionalProperties
from petstore_api.model.nullable_class import NullableClass
from petstore_api.model.nullable_shape import NullableShape
from petstore_api.model.nullable_string import NullableString
from petstore_api.model.number import Number
from petstore_api.model.number_only import NumberOnly
from petstore_api.model.number_with_validations import NumberWithValidations
from petstore_api.model.object_interface import ObjectInterface
from petstore_api.model.object_model_with_ref_props import ObjectModelWithRefProps
from petstore_api.model.object_with_decimal_properties import ObjectWithDecimalProperties
from petstore_api.model.object_with_difficultly_named_props import ObjectWithDifficultlyNamedProps
from petstore_api.model.object_with_inline_composition_property import ObjectWithInlineCompositionProperty
from petstore_api.model.object_with_validations import ObjectWithValidations
from petstore_api.model.order import Order
from petstore_api.model.parent_pet import ParentPet
from petstore_api.model.pet import Pet
from petstore_api.model.pig import Pig
from petstore_api.model.player import Player
from petstore_api.model.quadrilateral import Quadrilateral
from petstore_api.model.quadrilateral_interface import QuadrilateralInterface
from petstore_api.model.read_only_first import ReadOnlyFirst
from petstore_api.model.scalene_triangle import ScaleneTriangle
from petstore_api.model.shape import Shape
from petstore_api.model.shape_or_null import ShapeOrNull
from petstore_api.model.simple_quadrilateral import SimpleQuadrilateral
from petstore_api.model.some_object import SomeObject
from petstore_api.model.special_model_name import SpecialModelName
from petstore_api.model.string import String
from petstore_api.model.string_boolean_map import StringBooleanMap
from petstore_api.model.string_enum import StringEnum
from petstore_api.model.string_enum_with_default_value import StringEnumWithDefaultValue
from petstore_api.model.string_with_validation import StringWithValidation
from petstore_api.model.tag import Tag
from petstore_api.model.triangle import Triangle
from petstore_api.model.triangle_interface import TriangleInterface
from petstore_api.model.uuid_string import UUIDString
from petstore_api.model.user import User
from petstore_api.model.whale import Whale
from petstore_api.model.zebra import Zebra
