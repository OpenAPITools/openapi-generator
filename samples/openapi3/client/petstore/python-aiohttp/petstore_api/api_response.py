"""API response object."""

from __future__ import annotations
from typing import Any, Dict, Optional, Union

from aiohttp import ClientResponse
from pydantic import Field, StrictInt, StrictStr

class ApiResponse:
    """
    API response object returned by `_with_http_info` API methods.
    """

    status_code: Optional[StrictInt] = Field(None, description="HTTP status code")
    headers: Optional[Dict[StrictStr, StrictStr]] = Field(None, description="HTTP headers")
    data: Optional[Any] = Field(None, description="Deserialized data given the data type")
    raw_data: Union[None, str, bytes] = Field(
        title="Raw data (HTTP response body)",
        description="`bytes` if the OpenAPI response body format is 'binary', "
        "otherwise `str`. `None` if `_preload_content=False`.",
    )
    aiohttp_response: ClientResponse = Field(description="Raw aiohttp response for response streaming")

    def __init__(
        self,
        status_code=None,
        headers=None,
        data=None,
        *,
        raw_data,
        aiohttp_response,
    ) -> None:
        self.status_code = status_code
        self.headers = headers
        self.data = data
        self.raw_data = raw_data
        self.aiohttp_response = aiohttp_response


    async def read(self) -> Union[str, bytes]:
        """Read raw response body.

        If `preload_content=False`, this loads the entire response body into
        memory. If `preload_content=True`, the response body is already loaded
        earlier.
        """
        if self.raw_data is not None:
            return self.raw_data
        return await self.aiohttp_response.read()
