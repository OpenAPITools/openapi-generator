import os
import unittest
from unittest.mock import Mock, patch

import petstore_api


def get_field_from_formdata(formdata, name):
    return next(filter(lambda x: x[0]["name"] == name, formdata._fields))[-1]


class TestMultipleResponseTypes(unittest.TestCase):
    def setUpFiles(self):
        self.test_file_dir = os.path.join(os.path.dirname(__file__), "..", "testfiles")
        self.test_file_dir = os.path.realpath(self.test_file_dir)
        self.test_file_path = os.path.join(self.test_file_dir, "foo.png")

    def setUp(self):
        self.setUpFiles()

    def test_multipart_requests(self):
        mock_resp = Mock()
        mock_resp.return_value.read.return_value = b"some text"
        mock_resp.return_value.status_code = 200
        mock_resp.return_value.headers = {}

        marker = petstore_api.TestObjectForMultipartRequestsRequestMarker(
            name="name",
        )

        with patch("httpx.Client.request", mock_resp):
            with petstore_api.ApiClient() as api_client:
                fake_api = petstore_api.FakeApi(api_client)
                fake_api.test_object_for_multipart_requests(marker=marker)

        # success if no errors

    def test_multipart_requests_with_file_and_additional_properties(self):
        mock_resp = Mock()
        mock_resp.status_code = 200
        mock_resp.read = Mock(
            return_value=b'{"code": 200, "type": "success", "message": "OK"}'
        )
        mock_resp.headers = {"Content-Type": "application/json"}

        mock_request = Mock(return_value=mock_resp)
        with (
            open(self.test_file_path, "rb") as f,
            patch("httpx.Client.request", mock_request),
        ):
            with petstore_api.ApiClient() as api_client:
                fake_api = petstore_api.FakeApi(api_client)
                returned = fake_api.upload_file_with_additional_properties(
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

        mock_request.assert_called_once()

        formdata = mock_request.call_args_list[0].kwargs["data"]

        data_object = formdata["object"]
        data_count = formdata["count"]

        assert type(data_count) is str
        assert type(data_object) is str
