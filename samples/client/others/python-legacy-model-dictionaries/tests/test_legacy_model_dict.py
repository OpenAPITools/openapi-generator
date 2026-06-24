import functools
import json
import unittest
from typing import Any

import legacy_model_dict_client as client


class TestLegacyModelDictionaries(unittest.TestCase):
    def setUp(self) -> None:
        self.nested = client.NestedModel(camel_case="nested")
        self.model = client.LegacyModel(
            _continue="token",
            renamed="value",
            read_only_value="readonly",
            nested=self.nested,
            nested_list=[self.nested],
            nested_map={"key": self.nested},
            nested_lists=[[self.nested]],
            nested_maps=[{"key": self.nested}],
            map_of_lists={"key": [self.nested]},
            map_of_maps={"key": {"inner": self.nested}},
            nullable_value=None,
        )
        self.public_model_dict = {
            "inherited_value": None,
            "_continue": "token",
            "renamed": "value",
            "read_only_value": "readonly",
            "nested": {
                "camel_case": "nested",
                "nested_read_only_value": None,
            },
            "nested_list": [
                {
                    "camel_case": "nested",
                    "nested_read_only_value": None,
                }
            ],
            "nested_map": {
                "key": {
                    "camel_case": "nested",
                    "nested_read_only_value": None,
                }
            },
            "nested_lists": [[self.nested]],
            "nested_maps": [{"key": self.nested}],
            "map_of_lists": {"key": [self.nested]},
            "map_of_maps": {"key": {"inner": self.nested}},
            "nullable_value": None,
            "default_value": "default",
        }
        self.wire_model_dict = {
            "inheritedValue": None,
            "continue": "token",
            "ordinary": "value",
            "readOnlyValue": "readonly",
            "nested": {
                "camelCase": "nested",
                "nestedReadOnlyValue": None,
            },
            "nestedList": [
                {
                    "camelCase": "nested",
                    "nestedReadOnlyValue": None,
                }
            ],
            "nestedMap": {
                "key": {
                    "camelCase": "nested",
                    "nestedReadOnlyValue": None,
                }
            },
            "nestedLists": [[self.nested]],
            "nestedMaps": [{"key": self.nested}],
            "mapOfLists": {"key": [self.nested]},
            "mapOfMaps": {"key": {"inner": self.nested}},
            "nullableValue": None,
            "defaultValue": "default",
        }
        self.transport_model_dict = {
            "continue": "token",
            "ordinary": "value",
            "nested": {"camelCase": "nested"},
            "nestedList": [{"camelCase": "nested"}],
            "nestedMap": {"key": {"camelCase": "nested"}},
            "nestedLists": [[{"camelCase": "nested"}]],
            "nestedMaps": [{"key": {"camelCase": "nested"}}],
            "mapOfLists": {"key": [{"camelCase": "nested"}]},
            "mapOfMaps": {
                "key": {"inner": {"camelCase": "nested"}}
            },
            "nullableValue": None,
            "defaultValue": "default",
        }

    def test_model_projections(self) -> None:
        self.assertEqual(self.model.to_dict(), self.public_model_dict)
        self.assertEqual(
            self.model.to_dict(serialize=True),
            self.wire_model_dict,
        )
        self.assertEqual(
            json.loads(self.model.to_json()),
            self.transport_model_dict,
        )
        self.assertEqual(
            client.ApiClient().sanitize_for_serialization(self.model),
            self.transport_model_dict,
        )

    def test_wire_names_remain_inputs(self) -> None:
        model = client.LegacyModel.model_validate(
            {
                "continue": "token",
                "ordinary": "value",
                "readOnlyValue": "readonly",
                "nestedList": [{"camelCase": "nested"}],
            }
        )

        self.assertEqual(model._continue, "token")
        self.assertEqual(model.renamed, "value")
        self.assertEqual(model.read_only_value, "readonly")
        assert model.nested_list is not None
        self.assertEqual(model.nested_list[0].camel_case, "nested")

    def test_nested_containers_convert_one_level(self) -> None:
        public_dict = self.model.to_dict()
        self.assertIs(public_dict["nested_lists"][0][0], self.nested)
        self.assertIs(public_dict["nested_maps"][0]["key"], self.nested)
        self.assertIs(public_dict["map_of_lists"]["key"][0], self.nested)
        self.assertIs(
            public_dict["map_of_maps"]["key"]["inner"],
            self.nested,
        )

    def test_additional_properties_remain_transport_only(self) -> None:
        model = client.AdditionalPropertiesModel(declared_value="declared")
        model.additional_properties["dynamic"] = self.nested

        self.assertEqual(model.to_dict(), {"declared_value": "declared"})
        self.assertEqual(
            model.to_dict(serialize=True),
            {"declaredValue": "declared"},
        )
        expected_json = {
            "declaredValue": "declared",
            "dynamic": {
                "camelCase": "nested",
                "nestedReadOnlyValue": None,
            },
        }
        expected_request = {
            "declaredValue": "declared",
            "dynamic": {"camelCase": "nested"},
        }
        self.assertEqual(json.loads(model.to_json()), expected_json)
        self.assertEqual(
            client.ApiClient().sanitize_for_serialization(model),
            expected_request,
        )

    def test_composed_model_projections(self) -> None:
        api_client = client.ApiClient()
        for wrapper in (
            client.OneOfModel(self.model),
            client.AnyOfModel(self.model),
        ):
            self.assertEqual(wrapper.to_dict(), self.public_model_dict)
            self.assertEqual(
                wrapper.to_dict(serialize=True),
                self.wire_model_dict,
            )
            self.assertEqual(
                api_client.sanitize_for_serialization(wrapper),
                self.transport_model_dict,
            )
            self.assertEqual(
                json.loads(wrapper.to_json()),
                self.transport_model_dict,
            )

        for wrapper in (
            client.OneOfModel([self.nested]),
            client.AnyOfModel([self.nested]),
        ):
            self.assertEqual(
                wrapper.to_dict(),
                [
                    {
                        "camel_case": "nested",
                        "nested_read_only_value": None,
                    }
                ],
            )
            self.assertEqual(
                wrapper.to_dict(serialize=True),
                [
                    {
                        "camelCase": "nested",
                        "nestedReadOnlyValue": None,
                    }
                ],
            )
            self.assertEqual(
                api_client.sanitize_for_serialization(wrapper),
                [{"camelCase": "nested"}],
            )
            self.assertEqual(
                json.loads(wrapper.to_json()),
                [{"camelCase": "nested"}],
            )

        for wrapper in (
            client.OneOfModel({"key": self.nested}),
            client.AnyOfModel({"key": self.nested}),
        ):
            self.assertEqual(
                wrapper.to_dict(),
                {
                    "key": {
                        "camel_case": "nested",
                        "nested_read_only_value": None,
                    }
                },
            )
            self.assertEqual(
                wrapper.to_dict(serialize=True),
                {
                    "key": {
                        "camelCase": "nested",
                        "nestedReadOnlyValue": None,
                    }
                },
            )
            self.assertEqual(
                api_client.sanitize_for_serialization(wrapper),
                {"key": {"camelCase": "nested"}},
            )
            self.assertEqual(
                json.loads(wrapper.to_json()),
                {"key": {"camelCase": "nested"}},
            )

    def test_custom_to_dict_implementations(self) -> None:
        class ForeignModel:
            def to_dict(self) -> dict[str, object]:
                return {"foreign": True}

        class CustomLegacyModel(client.LegacyModel):
            def to_dict(self) -> dict[str, object]:  # type: ignore[override]
                return {"custom": True}

        class WrappedCustomLegacyModel(client.LegacyModel):
            def to_dict(
                self, serialize: bool = False
            ) -> dict[str, Any]:
                return {"wrapped": serialize}

        functools.update_wrapper(
            WrappedCustomLegacyModel.to_dict,
            client.LegacyModel.to_dict,
        )

        api_client = client.ApiClient()
        self.assertEqual(
            api_client.sanitize_for_serialization(ForeignModel()),
            {"foreign": True},
        )

        custom_model = CustomLegacyModel()
        self.assertEqual(
            api_client.sanitize_for_serialization(custom_model),
            {"custom": True},
        )
        self.assertEqual(json.loads(custom_model.to_json()), {"custom": True})
        for wrapper in (
            client.OneOfModel(custom_model),
            client.AnyOfModel(custom_model),
        ):
            self.assertEqual(wrapper.to_dict(), {"custom": True})
            self.assertEqual(
                api_client.sanitize_for_serialization(wrapper),
                {"custom": True},
            )

        wrapped_model = WrappedCustomLegacyModel()
        self.assertEqual(
            api_client.sanitize_for_serialization(wrapped_model),
            {"wrapped": False},
        )
        self.assertEqual(
            json.loads(wrapped_model.to_json()),
            {"wrapped": False},
        )

    def test_partial_constructed_model_emits_none(self) -> None:
        model = client.LegacyModel.model_construct()
        expected: dict[str, str | None] = {
            key: None for key in self.public_model_dict
        }
        expected["default_value"] = "default"
        self.assertEqual(model.to_dict(), expected)

    def test_inherited_generated_to_dict(self) -> None:
        class InheritedLegacyModel(client.LegacyModel):
            pass

        model = InheritedLegacyModel(
            **self.model.model_dump(by_alias=True),
        )
        self.assertEqual(model.to_dict(), self.public_model_dict)
        self.assertEqual(
            model.to_dict(serialize=True),
            self.wire_model_dict,
        )
        self.assertEqual(
            json.loads(model.to_json()),
            self.transport_model_dict,
        )
        self.assertEqual(
            client.ApiClient().sanitize_for_serialization(model),
            self.transport_model_dict,
        )
