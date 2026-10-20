from contextlib import contextmanager
import functools
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
import json
import threading
import unittest
from typing import Any, Iterator

import legacy_model_dict_client as client
from legacy_model_dict_client.exceptions import NotFoundException
from pydantic import ValidationError


class _LegacyModelsHandler(BaseHTTPRequestHandler):
    def do_GET(self) -> None:
        if self.path.startswith("/missing/"):
            self.send_error(404, "missing")
            return

        body = json.dumps(
            [{"ordinary": "response", "unknown": "ignored"}]
        ).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.send_header("X-Test-Header", "legacy")
        self.send_header("X-Test-Header", "duplicate")
        self.end_headers()
        self.wfile.write(body)

    def log_message(self, format: str, *args: Any) -> None:
        pass


@contextmanager
def _legacy_models_server() -> Iterator[str]:
    server = ThreadingHTTPServer(("127.0.0.1", 0), _LegacyModelsHandler)
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    try:
        yield "http://127.0.0.1:%d" % server.server_address[1]
    finally:
        server.shutdown()
        server.server_close()
        thread.join()


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

    def test_legacy_model_surface(self) -> None:
        self.assertEqual(client.LegacyModel.openapi_types["_continue"], "str")
        self.assertEqual(
            client.LegacyModel.attribute_map["_continue"],
            "continue",
        )
        self.assertEqual(repr(self.model), self.model.to_str())
        self.assertEqual(self.model, self.model.model_copy())
        self.assertNotEqual(
            self.model,
            self.model.model_copy(update={"renamed": "other"}),
        )

        with self.assertRaises(ValidationError):
            client.LegacyModel(unexpected="value")  # type: ignore[call-arg]

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
        other = client.AdditionalPropertiesModel(
            declared_value="declared",
        )
        other.additional_properties["dynamic"] = "other"

        self.assertEqual(model.to_dict(), {"declared_value": "declared"})
        self.assertNotEqual(model, other)
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

    def test_legacy_operation_shapes(self) -> None:
        with _legacy_models_server() as host:
            configuration = client.Configuration(host=host, no_proxy="*")
            with client.ApiClient(configuration) as api_client:
                api = client.DefaultApi(api_client)
                expected = [client.LegacyModel(renamed="response")]

                self.assertEqual(
                    api.list_legacy_models(_request_timeout=1),
                    expected,
                )
                self.assertEqual(
                    api.list_legacy_models(
                        _return_http_data_only=False,
                    ),
                    expected,
                )

                future: Any = api.list_legacy_models(async_req=True)
                self.assertEqual(future.get(), expected)

                raw_response: Any = api.list_legacy_models(
                    _preload_content=False,
                )
                self.assertEqual(raw_response.status, 200)
                self.assertEqual(
                    json.loads(raw_response.data),
                    [{"ordinary": "response", "unknown": "ignored"}],
                )
                raw_response.close()

                data, status, headers = (
                    api.list_legacy_models_with_http_info(
                        _request_timeout=(1, 2),
                    )
                )
                self.assertEqual(data, expected)
                self.assertEqual(status, 200)
                self.assertEqual(
                    headers.getlist("X-Test-Header"),
                    ["legacy", "duplicate"],
                )
                self.assertEqual(
                    api.list_legacy_models_with_http_info(
                        _return_http_data_only=True,
                    ),
                    expected,
                )

                raw_data: Any = api.list_legacy_models_with_http_info(
                    _preload_content=False,
                )
                self.assertEqual(raw_data.status, 200)
                self.assertEqual(
                    raw_data.headers.getlist("X-Test-Header"),
                    ["legacy", "duplicate"],
                )
                raw_data.close()

                raw_future: Any = (
                    api.list_legacy_models_with_http_info(
                        async_req=True,
                        _preload_content=False,
                    )
                )
                async_raw_data = raw_future.get()
                self.assertEqual(async_raw_data.status, 200)
                async_raw_data.close()

    def test_raw_operation_errors(self) -> None:
        with _legacy_models_server() as host:
            configuration = client.Configuration(
                host=host + "/missing",
                no_proxy="*",
            )
            with client.ApiClient(configuration) as api_client:
                api = client.DefaultApi(api_client)

                with self.assertRaises(NotFoundException):
                    api.list_legacy_models(_preload_content=False)

                future: Any = api.list_legacy_models(
                    async_req=True,
                    _preload_content=False,
                )
                with self.assertRaises(NotFoundException):
                    future.get()
