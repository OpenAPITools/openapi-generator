"""API response object."""

from __future__ import annotations
from typing import Any, Dict, Optional, TypeVar, Generic
from pydantic import Field, StrictInt, StrictStr

T = TypeVar("T")

class ApiResponse(Generic[T]):
    """
    API response object
    """

    status_code: Optional[StrictInt] = Field(None, description="HTTP status code")
    headers: Optional[Dict[StrictStr, StrictStr]] = Field(None, description="HTTP headers")
    data: Optional[T] = Field(None, description="Deserialized data given the data type")
    raw_data: Optional[Any] = Field(None, description="Raw data (HTTP response body)")

    def __init__(self,
                 status_code=None,
                 headers=None,
                 data=None,
                 raw_data=None):
        self.status_code = status_code
        self.headers = headers
        self.data = data
        self.raw_data = raw_data
