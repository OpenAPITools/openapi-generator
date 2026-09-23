import json
from typing import Any

import pytest

from petstore_api import (
    AnyOfContainer,
    AnyOfPig,
    ApiClient,
    BasquePig,
    NestedUnionContainer,
    OneOfContainer,
    Pig,
)


PIG = {"class'\"\\Name": "basque'\"\\pig\nkind", "color": "red"}


@pytest.mark.parametrize("model", [OneOfContainer, AnyOfContainer])
@pytest.mark.parametrize("value", [[PIG], {"pig": PIG}, "plain string", [1, 2], ["a"]])
def test_union_container_round_trip(model, value):
    for instance in (model.from_dict(value), model.from_json(json.dumps(value))):
        assert instance.to_dict() == value
        assert json.loads(instance.to_json()) == value
        assert ApiClient().sanitize_for_serialization(instance) == value


@pytest.mark.parametrize(
    "model,part", [(OneOfContainer, Pig), (AnyOfContainer, AnyOfPig)]
)
def test_union_container_explicit_models(model, part):
    item = part.from_dict(PIG)
    instance = model([item])
    assert instance.actual_instance[0] is item
    assert instance.to_dict() == [PIG]
    assert json.loads(instance.to_json()) == [PIG]


def test_nested_union_container_round_trip():
    value: Any = [{"pigs": [PIG]}]
    instance = NestedUnionContainer.from_dict(value)
    assert isinstance(instance.actual_instance, list)
    assert isinstance(instance.actual_instance[0]["pigs"][0], Pig)
    assert instance.to_dict() == value
    assert json.loads(instance.to_json()) == value
    assert ApiClient().sanitize_for_serialization(instance) == value


def test_anyof_container_preserves_first_match():
    instance = AnyOfContainer.from_json(json.dumps([{**PIG, "size": 1}]))
    assert isinstance(instance.actual_instance, list)
    assert isinstance(instance.actual_instance[0], AnyOfPig)
    assert isinstance(instance.actual_instance[0].actual_instance, BasquePig)


@pytest.mark.parametrize("model", [OneOfContainer, AnyOfContainer])
@pytest.mark.parametrize(
    "item", [{"required_integer_prop": 1}, {"required_integer_prop": 1, "string_prop": None}]
)
def test_union_container_ordinary_model_field_presence(model, item):
    instance = model.from_dict([item])
    assert instance.actual_instance[0].model_fields_set == set(item)
    assert instance.to_dict() == [item]
    assert ApiClient().sanitize_for_serialization(instance) == [item]


@pytest.mark.parametrize("model", [OneOfContainer, AnyOfContainer])
@pytest.mark.parametrize("value", [[], [None], {"pig": {}}, 42, True])
def test_union_container_validation(model, value):
    with pytest.raises(ValueError):
        model.from_dict(value)
