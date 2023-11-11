import unittest
from unittest.mock import patch, Mock

import petstore_api


class TestMultipleResponseTypes(unittest.TestCase):
    def setUp(self):
        self.api_client = petstore_api.ApiClient()
        self.fake_api = petstore_api.FakeApi(self.api_client)

    def test_204(self):
        mock_resp = Mock()
        mock_resp.status = 204
        mock_resp.data = b""
        mock_resp.getheaders.return_value = {}

        with patch(
            "petstore_api.api_client.ApiClient.call_api", return_value=mock_resp
        ):
            returned = self.fake_api.test_empty_and_non_empty_responses()

        assert returned is None

    def test_206(self):
        mock_resp = Mock()
        mock_resp.status = 206
        mock_resp.data = b"some text"
        mock_resp.getheaders.return_value = {}
        mock_resp.getheader = (
            lambda name: "text/plain" if name == "content-type" else Mock()
        )

        with patch(
            "petstore_api.api_client.ApiClient.call_api", return_value=mock_resp
        ):
            returned = self.fake_api.test_empty_and_non_empty_responses()

        assert returned == "some text"