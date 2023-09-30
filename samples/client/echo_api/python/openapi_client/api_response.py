"""API response object."""

from __future__ import annotations
from typing import Any, Dict, Optional
from typing import Callable, TypeVar, Generic
from pydantic import Field, StrictInt, StrictStr

class ApiResponse:
    """
    API response object
    """

    status_code: Optional[StrictInt] = Field(None, description="HTTP status code")
    headers: Optional[Dict[StrictStr, StrictStr]] = Field(None, description="HTTP headers")
    data: Optional[Any] = Field(None, description="Deserialized data given the data type")
    raw_data: Optional[Any] = Field(None, description="Raw data (HTTP response body)")

    def __init__(self,
                 status_code=None,
                 headers=None,
                 data=None,
                 raw_data=None) -> None:
        self.status_code = status_code
        self.headers = headers
        self.data = data
        self.raw_data = raw_data

class AsyncApiResponse:
    def __init__(self, thread) -> None:
        self._t = thread

    def get(self) -> ApiResponse:
        response = self._t.get()
        return response


T = TypeVar("T")

class AsyncResponse(Generic[T]):
    def __init__(self,
        response: AsyncApiResponse,
        deserializer: Callable[[ApiResponse], T],
    ) -> None:
        self._r = response
        self._d = deserializer

    def get(self) -> T:
        response = self._r.get()
        return self._d(response)
