import os
import unittest
from unittest.mock import Mock, patch

import petstore_api


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
