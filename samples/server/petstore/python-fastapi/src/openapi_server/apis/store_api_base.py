# coding: utf-8

from typing import ClassVar, Dict, List, Tuple  # noqa: F401

from pydantic import Field, StrictInt, StrictStr
from typing import Any, Dict
from typing_extensions import Annotated
from openapi_server.models.order import Order
from openapi_server.security_api import get_token_api_key

class BaseStoreApi:
    subclasses: ClassVar[Tuple] = ()

    def __init_subclass__(cls, **kwargs):
        super().__init_subclass__(**kwargs)
        BaseStoreApi.subclasses = BaseStoreApi.subclasses + (cls,)
    async def delete_order(
        self,
        orderId: Annotated[StrictStr, Field(description="ID of the order that needs to be deleted")],
    ) -> None:
        """For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors"""
        ...


    async def get_inventory(
        self,
    ) -> Dict[str, int]:
        """Returns a map of status codes to quantities"""
        ...


    async def get_order_by_id(
        self,
        orderId: Annotated[int, Field(le=5, strict=True, ge=1, description="ID of pet that needs to be fetched")],
    ) -> Order:
        """For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generate exceptions"""
        ...


    async def place_order(
        self,
        order: Annotated[Order, Field(description="order placed for purchasing the pet")],
    ) -> Order:
        """"""
        ...
