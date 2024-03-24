import json
import unittest
from unittest.mock import patch, Mock

import pytest

import petstore_api


class TestMultipleResponseTypes(unittest.TestCase):
    def setUp(self):
        self.api_client = petstore_api.ApiClient()
        self.fake_api = petstore_api.FakeApi(self.api_client)

    def test_multipart_requests(self):
        mock_resp = Mock()
        mock_resp.status = 200
        mock_resp.data = b"some text"
        mock_resp.headers = {}

        marker = petstore_api.TestObjectForMultipartRequestsRequestMarker(
            name="name",
        )

        with patch(
            "urllib3.PoolManager.urlopen", return_value=mock_resp
        ):
            returned = self.fake_api.test_object_for_multipart_requests(
                marker=marker
            )
        assert returned is None