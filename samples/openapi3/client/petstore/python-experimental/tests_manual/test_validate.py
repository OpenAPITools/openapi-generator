# coding: utf-8

from decimal import Decimal
import sys
import unittest

import petstore_api
from petstore_api.model.string_with_validation import StringWithValidation
from petstore_api.model.string_enum import StringEnum
from petstore_api.model.number_with_validations import NumberWithValidations
from petstore_api.model.array_holding_any_type import ArrayHoldingAnyType
from petstore_api.model.foo import Foo
from petstore_api.model.animal import Animal
from petstore_api.model.dog import Dog
from petstore_api.model.dog_all_of import DogAllOf
from petstore_api.model.boolean_enum import BooleanEnum
from petstore_api.model.pig import Pig
from petstore_api.model.danish_pig import DanishPig
from petstore_api.model.gm_fruit import GmFruit
from petstore_api.model.apple import Apple
from petstore_api.model.banana import Banana

from petstore_api.model_utils import AnyTypeSchema, StrSchema, IntOrFloatSchema

class TestValidate(unittest.TestCase):

    def test_str_validate(self):
        im = petstore_api.enums.InstantiationMetadata()
        _cls, path_to_schemas = StringWithValidation._validate('abcdefg', _instantiation_metadata=im)
        assert path_to_schemas == {('args[0]',): set([StringWithValidation, str])}

    def test_number_validate(self):
        im = petstore_api.enums.InstantiationMetadata()
        _cls, path_to_schemas = NumberWithValidations._validate(Decimal(11), _instantiation_metadata=im)
        assert path_to_schemas == {('args[0]',): set([NumberWithValidations, Decimal])}

    def test_str_enum_validate(self):
        im = petstore_api.enums.InstantiationMetadata()
        _cls, path_to_schemas = StringEnum._validate('placed', _instantiation_metadata=im)
        assert path_to_schemas == {('args[0]',): set([StringEnum])}

    def test_nullable_enum_validate(self):
        im = petstore_api.enums.InstantiationMetadata()
        _cls, path_to_schemas = StringEnum._validate(None, _instantiation_metadata=im)
        assert path_to_schemas == {('args[0]',): set([StringEnum])}

    def test_empty_list_validate(self):
        im = petstore_api.enums.InstantiationMetadata()
        _cls, path_to_schemas = ArrayHoldingAnyType._validate([], _instantiation_metadata=im)
        assert path_to_schemas == {('args[0]',): set([ArrayHoldingAnyType, list])}

    def test_list_validate(self):
        im = petstore_api.enums.InstantiationMetadata()
        _cls, path_to_schemas = ArrayHoldingAnyType._validate([Decimal(1), 'a'], _instantiation_metadata=im)
        assert path_to_schemas == {
            ('args[0]',): set([ArrayHoldingAnyType, list]),
            ('args[0]', 0): set([AnyTypeSchema, IntOrFloatSchema, Decimal]),
            ('args[0]', 1): set([AnyTypeSchema, StrSchema, str])
        }

    def test_empty_dict_validate(self):
        im = petstore_api.enums.InstantiationMetadata()
        _cls, path_to_schemas = Foo._validate({}, _instantiation_metadata=im)
        assert path_to_schemas == {('args[0]',): set([Foo, dict])}

    def test_dict_validate(self):
        im = petstore_api.enums.InstantiationMetadata()
        _cls, path_to_schemas = Foo._validate({'bar': 'a', 'additional': Decimal(0)}, _instantiation_metadata=im)
        assert path_to_schemas == {
            ('args[0]',): set([Foo, dict]),
            ('args[0]', 'bar'): set([StrSchema, str]),
            ('args[0]', 'additional'): set([AnyTypeSchema, IntOrFloatSchema, Decimal])
        }

    def test_discriminated_dict_validate(self):
        im = petstore_api.enums.InstantiationMetadata()
        _cls, path_to_schemas = Animal._validate(dict(className='Dog', color='black'), _instantiation_metadata=im)
        assert path_to_schemas == {
            ('args[0]',): set([Animal, Dog, DogAllOf, dict]),
            ('args[0]', 'className'): set([StrSchema, str, AnyTypeSchema]),
            ('args[0]', 'color'): set([StrSchema, str, AnyTypeSchema]),
        }

    def test_bool_enum_validate(self):
        im = petstore_api.enums.InstantiationMetadata()
        _cls, path_to_schemas = BooleanEnum._validate(True, _instantiation_metadata=im)
        assert path_to_schemas == {
            ('args[0]',): set([BooleanEnum])
        }

    def test_oneof_composition_pig_validate(self):
        im = petstore_api.enums.InstantiationMetadata()
        _cls, path_to_schemas = Pig._validate(dict(className='DanishPig'), _instantiation_metadata=im)
        assert path_to_schemas == {
            ('args[0]',): set([Pig, DanishPig, dict]),
            ('args[0]', 'className'): set([DanishPig.className]),
        }

    def test_anyof_composition_gm_fruit_validate(self):
        im = petstore_api.enums.InstantiationMetadata()
        _cls, path_to_schemas = GmFruit._validate(dict(cultivar='GoldenDelicious', lengthCm=Decimal(10)), _instantiation_metadata=im)
        assert path_to_schemas == {
            ('args[0]',): set([GmFruit, Apple, Banana, dict]),
            ('args[0]', 'cultivar'): set([Apple.cultivar, AnyTypeSchema, StrSchema, str]),
            ('args[0]', 'lengthCm'): set([AnyTypeSchema, IntOrFloatSchema, Decimal]),
        }


if __name__ == '__main__':
    unittest.main()
