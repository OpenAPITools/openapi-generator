import os
import unittest
from unittest.mock import AsyncMock, Mock, patch

import petstore_api


def get_field_from_formdata(formdata, name):
    return next(filter(lambda x: x[0]["name"] == name, formdata._fields))[-1]


class TestMultipleResponseTypes(unittest.IsolatedAsyncioTestCase):
    def setUpFiles(self):
        self.test_file_dir = os.path.join(os.path.dirname(__file__), "..", "testfiles")
        self.test_file_dir = os.path.realpath(self.test_file_dir)
        self.test_file_path = os.path.join(self.test_file_dir, "foo.png")

    def setUp(self):
        self.setUpFiles()

    async def test_multipart_requests(self):
        mock_resp = AsyncMock()
        mock_resp.return_value.read.return_value = b"some text"
        mock_resp.return_value.status = 200
        mock_resp.return_value.headers = {}

        marker = petstore_api.TestObjectForMultipartRequestsRequestMarker(
            name="name",
        )

        with patch("aiohttp.ClientSession.request", mock_resp):
            async with petstore_api.ApiClient() as api_client:
                fake_api = petstore_api.FakeApi(api_client)
                await fake_api.test_object_for_multipart_requests(marker=marker)

        # success if no errors

    async def test_multipart_requests_with_file_and_additional_properties(self):
        mock_resp = Mock()
        mock_resp.status = 200
        mock_resp.read = AsyncMock(
            return_value=b'{"code": 200, "type": "success", "message": "OK"}'
        )
        mock_resp.headers = {"Content-Type": "application/json"}

        mock_request = AsyncMock(return_value=mock_resp)
        with patch("aiohttp.ClientSession.request", mock_request):
            async with petstore_api.ApiClient() as api_client:
                fake_api = petstore_api.FakeApi(api_client)
                returned = await fake_api.upload_file_with_additional_properties(
                    file=self.test_file_path,
                    count=100,
                    object=petstore_api.UploadFileWithAdditionalPropertiesRequestObject(
                        name="foo"
                    ),
                )

        # response shape is actually petstore_api.models.api_response.ApiResponse,
        # but return type is annotated petstore_api.api_response.ApiResponse, thus
        # the type: ignores
        assert (
            returned.code == 200  # type: ignore
            and returned.type == "success"  # type: ignore
            and returned.message == "OK"  # type: ignore
        )

        mock_request.assert_called_once()

        formdata = mock_request.call_args_list[0].kwargs["data"]

        data_object = get_field_from_formdata(formdata, "object")
        data_count = get_field_from_formdata(formdata, "count")

        assert type(data_count) is str
        assert type(data_object) is str
