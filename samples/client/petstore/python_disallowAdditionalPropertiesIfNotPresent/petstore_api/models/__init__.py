# flake8: noqa

# import all models into this package
# if you have many models here with many references from one model to another this may
# raise a RecursionError
# to avoid this, import only the models that you directly need like:
# from from petstore_api.model.pet import Pet
# or import this package, but before doing it, use:
# import sys
# sys.setrecursionlimit(n)

from petstore_api.model.additional_properties_any_type import AdditionalPropertiesAnyType
from petstore_api.model.additional_properties_array import AdditionalPropertiesArray
from petstore_api.model.additional_properties_boolean import AdditionalPropertiesBoolean
from petstore_api.model.additional_properties_class import AdditionalPropertiesClass
from petstore_api.model.additional_properties_integer import AdditionalPropertiesInteger
from petstore_api.model.additional_properties_number import AdditionalPropertiesNumber
from petstore_api.model.additional_properties_object import AdditionalPropertiesObject
from petstore_api.model.additional_properties_string import AdditionalPropertiesString
from petstore_api.model.animal import Animal
from petstore_api.model.animal_farm import AnimalFarm
from petstore_api.model.api_response import ApiResponse
from petstore_api.model.array_of_array_of_number_only import ArrayOfArrayOfNumberOnly
from petstore_api.model.array_of_number_only import ArrayOfNumberOnly
from petstore_api.model.array_test import ArrayTest
from petstore_api.model.capitalization import Capitalization
from petstore_api.model.cat import Cat
from petstore_api.model.cat_all_of import CatAllOf
from petstore_api.model.category import Category
from petstore_api.model.child import Child
from petstore_api.model.child_all_of import ChildAllOf
from petstore_api.model.child_cat import ChildCat
from petstore_api.model.child_cat_all_of import ChildCatAllOf
from petstore_api.model.child_dog import ChildDog
from petstore_api.model.child_dog_all_of import ChildDogAllOf
from petstore_api.model.child_lizard import ChildLizard
from petstore_api.model.child_lizard_all_of import ChildLizardAllOf
from petstore_api.model.class_model import ClassModel
from petstore_api.model.client import Client
from petstore_api.model.dog import Dog
from petstore_api.model.dog_all_of import DogAllOf
from petstore_api.model.enum_arrays import EnumArrays
from petstore_api.model.enum_class import EnumClass
from petstore_api.model.enum_test import EnumTest
from petstore_api.model.file import File
from petstore_api.model.file_schema_test_class import FileSchemaTestClass
from petstore_api.model.format_test import FormatTest
from petstore_api.model.grandparent import Grandparent
from petstore_api.model.grandparent_animal import GrandparentAnimal
from petstore_api.model.has_only_read_only import HasOnlyReadOnly
from petstore_api.model.list import List
from petstore_api.model.map_test import MapTest
from petstore_api.model.mixed_properties_and_additional_properties_class import MixedPropertiesAndAdditionalPropertiesClass
from petstore_api.model.model200_response import Model200Response
from petstore_api.model.model_return import ModelReturn
from petstore_api.model.name import Name
from petstore_api.model.number_only import NumberOnly
from petstore_api.model.number_with_validations import NumberWithValidations
from petstore_api.model.object_model_with_ref_props import ObjectModelWithRefProps
from petstore_api.model.order import Order
from petstore_api.model.parent import Parent
from petstore_api.model.parent_all_of import ParentAllOf
from petstore_api.model.parent_pet import ParentPet
from petstore_api.model.pet import Pet
from petstore_api.model.player import Player
from petstore_api.model.read_only_first import ReadOnlyFirst
from petstore_api.model.special_model_name import SpecialModelName
from petstore_api.model.string_boolean_map import StringBooleanMap
from petstore_api.model.string_enum import StringEnum
from petstore_api.model.tag import Tag
from petstore_api.model.type_holder_default import TypeHolderDefault
from petstore_api.model.type_holder_example import TypeHolderExample
from petstore_api.model.user import User
from petstore_api.model.xml_item import XmlItem
