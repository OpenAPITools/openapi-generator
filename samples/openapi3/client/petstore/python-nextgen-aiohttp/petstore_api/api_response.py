from __future__ import annotations
from typing import Any, List, Optional
from pydantic import StrictInt, StrictStr

class ApiResponse:
    """
    API response object
    """

    status_code: Optional[StrictInt]
    headers: Optional[Dict[StrictStr, StrictStr]]
    data: Optional[Any]
    raw_data: Optional[Any]

    def __init__(self,
                 status_code=None,
                 headers=None,
                 data=None,
                 raw_data=None):
        self.status_code = status_code
        self.headers = headers
        self.data = data
        self.raw_data = data
