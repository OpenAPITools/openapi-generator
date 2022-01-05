# coding: utf-8

from collections import defaultdict
from decimal import Decimal
import sys
import typing
from unittest.mock import patch
import unittest

import petstore_api
from petstore_api.model.string_with_validation import StringWithValidation
from petstore_api.model.string_enum import StringEnum
from petstore_api.model.number_with_validations import NumberWithValidations
from petstore_api.model.array_holding_any_type import ArrayHoldingAnyType
from petstore_api.model.array_with_validations_in_items import ArrayWithValidationsInItems
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

from petstore_api.schemas import (
    AnyTypeSchema,
    StrSchema,
    NumberSchema,
    Schema,
    InstantiationMetadata,
    Int64Schema,
    StrBase,
    NumberBase,
    DictBase,
    ListBase,
    frozendict,
)

class TestValidateResults(unittest.TestCase):

    def test_str_validate(self):
        im = InstantiationMetadata()
        path_to_schemas = StringWithValidation._validate('abcdefg', _instantiation_metadata=im)
        assert path_to_schemas == {('args[0]',): set([StringWithValidation, str])}

    def test_number_validate(self):
        im = InstantiationMetadata()
        path_to_schemas = NumberWithValidations._validate(Decimal(11), _instantiation_metadata=im)
        assert path_to_schemas == {('args[0]',): set([NumberWithValidations, Decimal])}

    def test_str_enum_validate(self):
        im = InstantiationMetadata()
        path_to_schemas = StringEnum._validate('placed', _instantiation_metadata=im)
        assert path_to_schemas == {('args[0]',): set([StringEnum])}

    def test_nullable_enum_validate(self):
        im = InstantiationMetadata()
        path_to_schemas = StringEnum._validate(None, _instantiation_metadata=im)
        assert path_to_schemas == {('args[0]',): set([StringEnum])}

    def test_empty_list_validate(self):
        im = InstantiationMetadata()
        path_to_schemas = ArrayHoldingAnyType._validate((), _instantiation_metadata=im)
        assert path_to_schemas == {('args[0]',): set([ArrayHoldingAnyType, tuple])}

    def test_list_validate(self):
        im = InstantiationMetadata()
        path_to_schemas = ArrayHoldingAnyType._validate((Decimal(1), 'a'), _instantiation_metadata=im)
        assert path_to_schemas == {
            ('args[0]',): set([ArrayHoldingAnyType, tuple]),
            ('args[0]', 0): set([AnyTypeSchema, Decimal]),
            ('args[0]', 1): set([AnyTypeSchema, str])
        }

    def test_empty_dict_validate(self):
        im = InstantiationMetadata()
        path_to_schemas = Foo._validate(frozendict({}), _instantiation_metadata=im)
        assert path_to_schemas == {('args[0]',): set([Foo, frozendict])}

    def test_dict_validate(self):
        im = InstantiationMetadata()
        path_to_schemas = Foo._validate(frozendict({'bar': 'a', 'additional': Decimal(0)}), _instantiation_metadata=im)
        assert path_to_schemas == {
            ('args[0]',): set([Foo, frozendict]),
            ('args[0]', 'bar'): set([StrSchema, str]),
            ('args[0]', 'additional'): set([AnyTypeSchema, Decimal])
        }

    def test_discriminated_dict_validate(self):
        im = InstantiationMetadata()
        path_to_schemas = Animal._validate(frozendict(className='Dog', color='black'), _instantiation_metadata=im)
        assert path_to_schemas == {
            ('args[0]',): set([Animal, Dog, DogAllOf, frozendict]),
            ('args[0]', 'className'): set([StrSchema, AnyTypeSchema, str]),
            ('args[0]', 'color'): set([StrSchema, AnyTypeSchema, str]),
        }

    def test_bool_enum_validate(self):
        im = InstantiationMetadata()
        path_to_schemas = BooleanEnum._validate(True, _instantiation_metadata=im)
        assert path_to_schemas == {
            ('args[0]',): set([BooleanEnum])
        }

    def test_oneof_composition_pig_validate(self):
        im = InstantiationMetadata()
        path_to_schemas = Pig._validate(frozendict(className='DanishPig'), _instantiation_metadata=im)
        assert path_to_schemas == {
            ('args[0]',): set([Pig, DanishPig, frozendict]),
            ('args[0]', 'className'): set([DanishPig.className, AnyTypeSchema, str]),
        }

    def test_anyof_composition_gm_fruit_validate(self):
        im = InstantiationMetadata()
        path_to_schemas = GmFruit._validate(frozendict(cultivar='GoldenDelicious', lengthCm=Decimal(10)), _instantiation_metadata=im)
        assert path_to_schemas == {
            ('args[0]',): set([GmFruit, Apple, Banana, frozendict]),
            ('args[0]', 'cultivar'): set([Apple.cultivar, AnyTypeSchema, str]),
            ('args[0]', 'lengthCm'): set([AnyTypeSchema, NumberSchema, Decimal]),
        }

class TestValidateCalls(unittest.TestCase):
    def test_empty_list_validate(self):
        return_value = {('args[0]',): set([ArrayHoldingAnyType, tuple])}
        with patch.object(Schema, '_validate', return_value=return_value) as mock_validate:
           instance = ArrayHoldingAnyType([])
           assert mock_validate.call_count == 1

        with patch.object(Schema, '_validate', return_value=return_value) as mock_validate:
           ArrayHoldingAnyType._from_openapi_data([])
           assert mock_validate.call_count == 1

    def test_empty_dict_validate(self):
        return_value = {('args[0]',): set([Foo, frozendict])}
        with patch.object(Schema, '_validate', return_value=return_value) as mock_validate:
           instance = Foo({})
           assert mock_validate.call_count == 1

        with patch.object(Schema, '_validate', return_value=return_value) as mock_validate:
           Foo._from_openapi_data({})
           assert mock_validate.call_count == 1

    def test_list_validate_direct_instantiation(self):
        expected_call_by_index = {
            0: [
                ArrayWithValidationsInItems,
                ((Decimal('7'),),),
                InstantiationMetadata(path_to_item=('args[0]',))
            ],
            1: [
                ArrayWithValidationsInItems._items,
                (Decimal('7'),),
                InstantiationMetadata(path_to_item=('args[0]', 0))
            ]
        }
        call_index = 0
        result_by_call_index = {
            0: defaultdict(set, [( ('args[0]',), set([ArrayWithValidationsInItems, tuple]))] ),
            1: defaultdict(set, [( ('args[0]', 0), set([ArrayWithValidationsInItems._items, Decimal]) )] ),
        }

        @classmethod
        def new_validate(cls, *args, _instantiation_metadata: typing.Optional[InstantiationMetadata] = None):
            nonlocal call_index
            assert [cls, args, _instantiation_metadata] == expected_call_by_index[call_index]
            result = result_by_call_index.get(call_index)
            call_index += 1
            if result is None:
                raise petstore_api.ApiValueError('boom')
            return result

        with patch.object(Schema, '_validate', new=new_validate):
            ArrayWithValidationsInItems([7])

    def test_list_validate_direct_instantiation_cast_item(self):
        # validation is skipped if items are of the correct type
        expected_call_by_index = {
            0: [
                ArrayWithValidationsInItems,
                ((Decimal('7'),),),
                InstantiationMetadata(path_to_item=('args[0]',))
            ],
        }
        call_index = 0
        result_by_call_index = {
            0: defaultdict(set, [( ('args[0]',), set([ArrayWithValidationsInItems, tuple]))] ),
        }

        @classmethod
        def new_validate(cls, *args, _instantiation_metadata: typing.Optional[InstantiationMetadata] = None):
            nonlocal call_index
            assert [cls, args, _instantiation_metadata] == expected_call_by_index[call_index]
            result = result_by_call_index.get(call_index)
            call_index += 1
            if result is None:
                raise petstore_api.ApiValueError('boom')
            return result

        item = ArrayWithValidationsInItems._items(7)
        with patch.object(Schema, '_validate', new=new_validate):
            ArrayWithValidationsInItems([item])

    def test_list_validate_from_openai_data_instantiation(self):
        expected_call_by_index = {
            0: [
                ArrayWithValidationsInItems,
                ((Decimal('7'),),),
                InstantiationMetadata(path_to_item=('args[0]',), from_server=True)
            ],
            1: [
                ArrayWithValidationsInItems._items,
                (Decimal('7'),),
                InstantiationMetadata(path_to_item=('args[0]', 0), from_server=True)
            ]
        }
        call_index = 0
        result_by_call_index = {
            0: defaultdict(set, [( ('args[0]',), set([ArrayWithValidationsInItems, tuple]))] ),
            1: defaultdict(set, [( ('args[0]', 0), set([ArrayWithValidationsInItems._items, Decimal]) )] ),
        }

        @classmethod
        def new_validate(cls, *args, _instantiation_metadata: typing.Optional[InstantiationMetadata] = None):
            nonlocal call_index
            assert [cls, args, _instantiation_metadata] == expected_call_by_index[call_index]
            result = result_by_call_index.get(call_index)
            call_index += 1
            if result is None:
                raise petstore_api.ApiValueError('boom')
            return result

        with patch.object(Schema, '_validate', new=new_validate):
            ArrayWithValidationsInItems._from_openapi_data([7])

    def test_dict_validate_direct_instantiation(self):
        expected_call_by_index = {
            0: [
                Foo,
                (frozendict({'bar': 'a'}),),
                InstantiationMetadata(path_to_item=('args[0]',))
            ],
            1: [
                StrSchema,
                ('a',),
                InstantiationMetadata(path_to_item=('args[0]', 'bar'))
            ]
        }
        call_index = 0
        result_by_call_index = {
            0: defaultdict(set, [( ('args[0]',), set([Foo, frozendict]))] ),
            1: defaultdict(set, [( ('args[0]', 'bar'), set([StrSchema, str]) )] ),
        }

        @classmethod
        def new_validate(cls, *args, _instantiation_metadata: typing.Optional[InstantiationMetadata] = None):
            nonlocal call_index
            assert [cls, args, _instantiation_metadata] == expected_call_by_index[call_index]
            result = result_by_call_index.get(call_index)
            call_index += 1
            if result is None:
                raise petstore_api.ApiValueError('boom')
            return result

        with patch.object(Schema, '_validate', new=new_validate):
            Foo(bar='a')

    def test_dict_validate_direct_instantiation_cast_item(self):
        expected_call_by_index = {
            0: [
                Foo,
                (frozendict({'bar': 'a'}),),
                InstantiationMetadata(path_to_item=('args[0]',))
            ],
        }
        call_index = 0
        result_by_call_index = {
            0: defaultdict(set, [( ('args[0]',), set([Foo, frozendict]))] ),
        }

        @classmethod
        def new_validate(cls, *args, _instantiation_metadata: typing.Optional[InstantiationMetadata] = None):
            nonlocal call_index
            assert [cls, args, _instantiation_metadata] == expected_call_by_index[call_index]
            result = result_by_call_index.get(call_index)
            call_index += 1
            if result is None:
                raise petstore_api.ApiValueError('boom')
            return result

        bar = StrSchema('a')
        with patch.object(Schema, '_validate', new=new_validate):
            Foo(bar=bar)

    def test_dict_validate_from_openapi_data_instantiation(self):
        expected_call_by_index = {
            0: [
                Foo,
                (frozendict({'bar': 'a'}),),
                InstantiationMetadata(path_to_item=('args[0]',), from_server=True)
            ],
            1: [
                StrSchema,
                ('a',),
                InstantiationMetadata(path_to_item=('args[0]', 'bar'), from_server=True)
            ]
        }
        call_index = 0
        result_by_call_index = {
            0: defaultdict(set, [( ('args[0]',), set([Foo, frozendict]))] ),
            1: defaultdict(set, [( ('args[0]', 'bar'), set([StrSchema, str]) )] ),
        }

        @classmethod
        def new_validate(cls, *args, _instantiation_metadata: typing.Optional[InstantiationMetadata] = None):
            nonlocal call_index
            assert [cls, args, _instantiation_metadata] == expected_call_by_index[call_index]
            result = result_by_call_index.get(call_index)
            call_index += 1
            if result is None:
                raise petstore_api.ApiValueError('boom')
            return result

        with patch.object(Schema, '_validate', new=new_validate):
            Foo._from_openapi_data({'bar': 'a'})


if __name__ == '__main__':
    unittest.main()
