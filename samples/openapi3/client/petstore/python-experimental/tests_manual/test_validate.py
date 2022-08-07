# coding: utf-8

from collections import defaultdict
from decimal import Decimal
import typing
from unittest.mock import patch, call
import unittest

import petstore_api
from petstore_api.model.string_with_validation import StringWithValidation
from petstore_api.model.string_enum import StringEnum
from petstore_api.model.number_with_validations import NumberWithValidations
from petstore_api.model.array_holding_any_type import ArrayHoldingAnyType
from petstore_api.model.array_with_validations_in_items import (
    ArrayWithValidationsInItems,
)
from petstore_api.model.foo import Foo
from petstore_api.model.animal import Animal
from petstore_api.model.dog import Dog
from petstore_api.model.boolean_enum import BooleanEnum
from petstore_api.model.pig import Pig
from petstore_api.model.danish_pig import DanishPig
from petstore_api.model.gm_fruit import GmFruit
from petstore_api.model.apple import Apple
from petstore_api.model.banana import Banana

from petstore_api.schemas import (
    AnyTypeSchema,
    BoolClass,
    NoneClass,
    StrSchema,
    NumberSchema,
    Schema,
    ValidationMetadata,
    frozendict,
)


class TestValidateResults(unittest.TestCase):
    def test_str_validate(self):
        vm = ValidationMetadata()
        path_to_schemas = StringWithValidation._validate(
            "abcdefg", validation_metadata=vm
        )
        assert path_to_schemas == {("args[0]",): set([StringWithValidation, str])}

    def test_number_validate(self):
        vm = ValidationMetadata()
        path_to_schemas = NumberWithValidations._validate(
            Decimal(11), validation_metadata=vm
        )
        assert path_to_schemas == {("args[0]",): set([NumberWithValidations, Decimal])}

    def test_str_enum_validate(self):
        vm = ValidationMetadata()
        path_to_schemas = StringEnum._validate("placed", validation_metadata=vm)
        assert path_to_schemas == {("args[0]",): {str, StringEnum}}

    def test_nullable_enum_validate(self):
        vm = ValidationMetadata()
        path_to_schemas = StringEnum._validate(NoneClass.NONE, validation_metadata=vm)
        assert path_to_schemas == {("args[0]",): {NoneClass, StringEnum}}

    def test_empty_list_validate(self):
        vm = ValidationMetadata()
        path_to_schemas = ArrayHoldingAnyType._validate((), validation_metadata=vm)
        assert path_to_schemas == {("args[0]",): set([ArrayHoldingAnyType, tuple])}

    def test_list_validate(self):
        vm = ValidationMetadata()
        path_to_schemas = ArrayHoldingAnyType._validate(
            (Decimal(1), "a"), validation_metadata=vm
        )
        assert path_to_schemas == {
            ("args[0]",): set([ArrayHoldingAnyType, tuple]),
            ("args[0]", 0): set([AnyTypeSchema, Decimal]),
            ("args[0]", 1): set([AnyTypeSchema, str]),
        }

    def test_empty_dict_validate(self):
        vm = ValidationMetadata()
        path_to_schemas = Foo._validate(frozendict({}), validation_metadata=vm)
        assert path_to_schemas == {("args[0]",): set([Foo, frozendict])}

    def test_dict_validate(self):
        vm = ValidationMetadata()
        path_to_schemas = Foo._validate(
            frozendict({"bar": "a", "additional": Decimal(0)}),
            validation_metadata=vm,
        )
        assert path_to_schemas == {
            ("args[0]",): set([Foo, frozendict]),
            ("args[0]", "bar"): set([StrSchema, str]),
            ("args[0]", "additional"): set([AnyTypeSchema, Decimal]),
        }

    def test_discriminated_dict_validate(self):
        vm = ValidationMetadata()
        path_to_schemas = Animal._validate(
            frozendict(className="Dog", color="black"), validation_metadata=vm
        )
        assert path_to_schemas == {
            ("args[0]",): set([Animal, Dog, Dog._composed_schemas['allOf'][1], frozendict]),
            ("args[0]", "className"): set([StrSchema, AnyTypeSchema, str]),
            ("args[0]", "color"): set([StrSchema, AnyTypeSchema, str]),
        }

    def test_bool_enum_validate(self):
        vm = ValidationMetadata()
        path_to_schemas = BooleanEnum._validate(BoolClass.TRUE, validation_metadata=vm)
        assert path_to_schemas == {("args[0]",): {BoolClass, BooleanEnum}}

    def test_oneof_composition_pig_validate(self):
        vm = ValidationMetadata()
        path_to_schemas = Pig._validate(
            frozendict(className="DanishPig"), validation_metadata=vm
        )
        assert path_to_schemas == {
            ("args[0]",): set([Pig, DanishPig, frozendict]),
            ("args[0]", "className"): set([DanishPig.className, AnyTypeSchema, str]),
        }

    def test_anyof_composition_gm_fruit_validate(self):
        vm = ValidationMetadata()
        path_to_schemas = GmFruit._validate(
            frozendict(cultivar="GoldenDelicious", lengthCm=Decimal(10)),
            validation_metadata=vm,
        )
        assert path_to_schemas == {
            ("args[0]",): set([GmFruit, Apple, Banana, frozendict]),
            ("args[0]", "cultivar"): set([Apple.cultivar, AnyTypeSchema, str]),
            ("args[0]", "lengthCm"): set([AnyTypeSchema, NumberSchema, Decimal]),
        }


class TestValidateCalls(unittest.TestCase):
    def test_empty_list_validate(self):
        return_value = {("args[0]",): set([ArrayHoldingAnyType, tuple])}
        with patch.object(
            Schema, "_validate", return_value=return_value
        ) as mock_validate:
            ArrayHoldingAnyType([])
            assert mock_validate.call_count == 1

        with patch.object(
            Schema, "_validate", return_value=return_value
        ) as mock_validate:
            ArrayHoldingAnyType._from_openapi_data([])
            assert mock_validate.call_count == 1

    def test_empty_dict_validate(self):
        return_value = {("args[0]",): set([Foo, frozendict])}
        with patch.object(
            Schema, "_validate", return_value=return_value
        ) as mock_validate:
            Foo({})
            assert mock_validate.call_count == 1

        with patch.object(
            Schema, "_validate", return_value=return_value
        ) as mock_validate:
            Foo._from_openapi_data({})
            assert mock_validate.call_count == 1

    def test_list_validate_direct_instantiation(self):
        results = [
            {("args[0]",): {ArrayWithValidationsInItems, tuple}},
            {("args[0]", 0): {ArrayWithValidationsInItems._items, Decimal}}
        ]
        with patch.object(Schema, "_validate", side_effect=results) as mock_validate:
            ArrayWithValidationsInItems([7])
            calls = [
                call(
                    (Decimal("7"),),
                    validation_metadata=ValidationMetadata(path_to_item=("args[0]",))
                ),
                call(
                    Decimal("7"),
                    ValidationMetadata(path_to_item=("args[0]", 0))
                )
            ]
            mock_validate.assert_has_calls(
                calls
            )

    def test_list_validate_direct_instantiation_cast_item(self):
        # item validation is skipped if items are of the correct type
        item = ArrayWithValidationsInItems._items(7)
        return_value = {("args[0]",): {ArrayWithValidationsInItems, tuple}}
        with patch.object(Schema, "_validate", return_value=return_value) as mock_validate:
            ArrayWithValidationsInItems([item])
            mock_validate.assert_called_once_with(
                tuple([Decimal('7')]),
                validation_metadata=ValidationMetadata(
                    validated_path_to_schemas={('args[0]', 0): {ArrayWithValidationsInItems._items, Decimal}}
                )
            )

    def test_list_validate_from_openai_data_instantiation(self):

        results = [
            {("args[0]",): {ArrayWithValidationsInItems, tuple}},
            {("args[0]", 0): {ArrayWithValidationsInItems._items, Decimal}}
        ]
        with patch.object(Schema, "_validate", side_effect=results) as mock_validate:
            ArrayWithValidationsInItems._from_openapi_data([7])
            calls = [
                call(
                    (Decimal("7"),),
                    validation_metadata=ValidationMetadata(path_to_item=("args[0]",), from_server=True)
                ),
                call(
                    Decimal("7"),
                    ValidationMetadata(path_to_item=("args[0]", 0), from_server=True)
                )
            ]
            mock_validate.assert_has_calls(
                calls
            )

    def test_dict_validate_direct_instantiation(self):
        call_results = [
            {("args[0]",): {Foo, frozendict}},
            {("args[0]", "bar"): {StrSchema, str}}
        ]
        with patch.object(Schema, "_validate", side_effect=call_results) as mock_validate:
            Foo(bar="a")
            calls = [
                call(
                    frozendict({"bar": "a"}),
                    validation_metadata=ValidationMetadata(path_to_item=("args[0]",)),
                ),
                call(
                    "a",
                    ValidationMetadata(path_to_item=("args[0]", "bar")),
                ),
            ]
            mock_validate.assert_has_calls(
                calls
            )

    def test_dict_validate_direct_instantiation_cast_item(self):
        bar = StrSchema("a")
        return_value = {
            ("args[0]",): {Foo, frozendict}
        }
        # only the Foo dict is validated because the bar property value was already validated
        with patch.object(Schema, "_validate", return_value=return_value) as mock_validate:
            Foo(bar=bar)
            mock_validate.assert_called_once_with(
                frozendict(dict(bar='a')),
                validation_metadata=ValidationMetadata(
                    validated_path_to_schemas={('args[0]', 'bar'): {str, StrSchema}}
                )
            )

    def test_dict_validate_from_openapi_data_instantiation(self):

        return_values = [
            {("args[0]",): {Foo, frozendict}},
            {("args[0]", 'bar'): {StrSchema, str}}
        ]
        with patch.object(Schema, "_validate", side_effect=return_values) as mock_validate:
            Foo._from_openapi_data({"bar": "a"})
            calls = [
                call(
                    frozendict({"bar": "a"}),
                    validation_metadata=ValidationMetadata(path_to_item=("args[0]",), from_server=True),
                ),
                call(
                    "a",
                    ValidationMetadata(path_to_item=("args[0]", "bar"), from_server=True),
                ),
            ]
            mock_validate.assert_has_calls(
                calls
            )


if __name__ == "__main__":
    unittest.main()
