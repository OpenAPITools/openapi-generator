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
from petstore_api.model.pig import Pig
from petstore_api.model.danish_pig import DanishPig
from petstore_api.model_utils import AnyTypeSchema, StrSchema

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
            ('args[0]', 0): set([AnyTypeSchema, Decimal]),
            ('args[0]', 1): set([AnyTypeSchema, str])
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
            ('args[0]', 'additional'): set([AnyTypeSchema, Decimal])
        }

    # TODO: add bool, None, object w/ discriminator, composition
    def test_discriminated_dict_validate(self):
        im = petstore_api.enums.InstantiationMetadata()
        _cls, path_to_schemas = Pig._validate({'className': 'DanishPig'}, _instantiation_metadata=im)
        assert path_to_schemas == {
            ('args[0]',): set([Pig, DanishPig, dict]),
            ('args[0]', 'className'): set([StrSchema, str]),
        }


if __name__ == '__main__':
    unittest.main()
