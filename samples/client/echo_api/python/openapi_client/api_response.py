"""API response object."""

from __future__ import annotations
from typing import Any, Dict, Optional, Union
import warnings

from pydantic import Field, StrictInt, StrictStr
from urllib3 import HTTPResponse

class ApiResponse:
    """
    API response object returned by `_with_http_info` API methods.
    """

    status_code: Optional[StrictInt] = Field(None, description="HTTP status code")
    headers: Optional[Dict[StrictStr, StrictStr]] = Field(None, description="HTTP headers")
    data: Optional[Any] = Field(None, description="Deserialized data given the data type")
    _raw_data: Union[None, str, bytes] = Field(
        title="Raw data (HTTP response body)",
        description="`bytes` if the OpenAPI response body format is 'binary', "
        "otherwise `str`. `None` if `_preload_content=False`.",
    )
    urllib3_response: HTTPResponse = Field(description="Raw urllib3 response for response streaming")

    def __init__(
        self,
        status_code=None,
        headers=None,
        data=None,
        *,
        raw_data,
        urllib3_response,
    ) -> None:
        self.status_code = status_code
        self.headers = headers
        self.data = data
        self._raw_data = raw_data
        self.urllib3_response = urllib3_response

    @property
    def raw_data(self) -> Union[str, bytes]:
        """Get raw response body.

        If `preload_content=False`, this loads the entire response body into
        memory. If `preload_content=True`, the response body is already loaded
        earlier.
        To stream responses, read from `self.urllib3_response`.

        :return: `str` if `preload_content=True`, `bytes` if not.
        """
        if self._raw_data is None:
            warnings.warn(
                "If `preload_content=False`, `ApiResponse.raw_data` will change to "
                "`None` in a future version. Use `ApiResponse.read()` instead.",
                DeprecationWarning,
            )
            return self.urllib3_response.data
        return self._raw_data

    def read(self) -> Union[str, bytes]:
        """Read raw response body.

        If `preload_content=False`, this loads the entire response body into
        memory. If `preload_content=True`, the response body is already loaded
        earlier.
        """
        if self._raw_data is not None:
            return self._raw_data
        return self.urllib3_response.data
