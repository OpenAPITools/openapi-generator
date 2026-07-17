import json
import json
import os
import unittest
from unittest.mock import Mock, patch

import petstore_api
from petstore_api.rest import RESTClientObject


class TestProxyConnection(unittest.TestCase):
    def test_no_proxy_selects_direct_connection(self):
        cases = [
            ("https://api.example.com", "example.com", True),
            ("https://api.example.com:8443", "example.com:8443", True),
            ("https://example.com:443", "example.com:8443", False),
            ("https://10.2.3.4", "10.0.0.0/8", True),
            ("https://[2001:db8::1]", "2001:db8::/32", True),
            ("https://api.example.net", "*", True),
            ("https://api.example.net", "example.com", False),
        ]
        for host, no_proxy, bypasses_proxy in cases:
            with self.subTest(host=host, no_proxy=no_proxy):
                config = petstore_api.Configuration(
                    host=host,
                    proxy="http://proxy.example",
                    no_proxy=no_proxy,
                )
                with (
                    patch("petstore_api.rest.urllib3.PoolManager") as direct,
                    patch("petstore_api.rest.urllib3.ProxyManager") as proxied,
                ):
                    RESTClientObject(config)

                if bypasses_proxy:
                    direct.assert_called_once()
                    proxied.assert_not_called()
                else:
                    direct.assert_not_called()
                    proxied.assert_called_once()


class TestYamlRequestBodies(unittest.TestCase):
    def setUp(self):
        self.rest_client = RESTClientObject(
            petstore_api.Configuration(proxy="")
        )
        self.request_mock = Mock(
            return_value=Mock(status=200, reason="OK")
        )
        self.rest_client.pool_manager = Mock(request=self.request_mock)

    def test_structured_yaml_body_is_json_encoded(self):
        body = {"apiVersion": "example.com/v1", "kind": "Example"}
        for content_type in (
            "application/yaml",
            "application/apply-patch+yaml; charset=utf-8",
        ):
            with self.subTest(content_type=content_type):
                self.request_mock.reset_mock()
                self.rest_client.request(
                    "PATCH",
                    "https://api.example",
                    headers={"Content-Type": content_type},
                    body=body,
                )

                self.request_mock.assert_called_once()
                request_body = self.request_mock.call_args.kwargs["body"]
                self.assertEqual(json.loads(request_body), body)

    def test_serialized_yaml_body_is_passed_through(self):
        body = "apiVersion: example.com/v1\nkind: Example\n"
        self.rest_client.request(
            "PATCH",
            "https://api.example",
            headers={"Content-Type": "application/yaml"},
            body=body,
        )

        self.request_mock.assert_called_once()
        request_body = self.request_mock.call_args.kwargs["body"]
        self.assertEqual(request_body, body)


class TestMultipleResponseTypes(unittest.TestCase):
    def setUpFiles(self):
        self.test_file_dir = os.path.join(os.path.dirname(__file__), "..", "testfiles")
        self.test_file_dir = os.path.realpath(self.test_file_dir)
        self.test_file_path = os.path.join(self.test_file_dir, "pix.gif")

    def setUp(self):
        self.api_client = petstore_api.ApiClient()
        self.fake_api = petstore_api.FakeApi(self.api_client)
        self.setUpFiles()

    def test_multipart_requests(self):
        mock_resp = Mock()
        mock_resp.status = 200
        mock_resp.data = b"some text"
        mock_resp.headers = {}

        marker = petstore_api.TestObjectForMultipartRequestsRequestMarker(
            name="name",
        )

        with patch("urllib3.PoolManager.urlopen", return_value=mock_resp):
            returned = self.fake_api.test_object_for_multipart_requests(marker=marker)
        assert returned is None

    def test_multipart_requests_with_file_and_additional_properties(self):
        mock_resp = Mock()
        mock_resp.status = 200
        mock_resp.data = b'{"code": 200, "type": "success", "message": "OK"}'
        mock_resp.headers = {"Content-Type": "application/json"}
        with open(self.test_file_path, "rb") as f, patch(
            "urllib3.PoolManager.urlopen", return_value=mock_resp
        ):
            returned = self.fake_api.upload_file_with_additional_properties(
                file=(self.test_file_path, f.read()),
                count=100,
                object=petstore_api.UploadFileWithAdditionalPropertiesRequestObject(
                    name="foo"
                ),
            )

        assert (
            returned.code == 200
            and returned.type == "success"
            and returned.message == "OK"
        )

        # if the request is successful, both int and dict body parameters were
        # successfully serialized
