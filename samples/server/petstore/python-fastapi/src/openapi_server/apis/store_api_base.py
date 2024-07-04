# coding: utf-8

from typing import ClassVar, Dict, List, Tuple  # noqa: F401

from openapi_server.models.order import Order
from openapi_server.security_api import get_token_api_key

class BaseStoreApi:
    subclasses: ClassVar[Tuple] = ()

    def __init_subclass__(cls, **kwargs):
        super().__init_subclass__(**kwargs)
        BaseStoreApi.subclasses = BaseStoreApi.subclasses + (cls,)
    def delete_order(
        self,
        orderId: str,
    ) -> None:
        """For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors"""
        ...


    def get_inventory(
        self,
    ) -> Dict[str, int]:
        """Returns a map of status codes to quantities"""
        ...


    def get_order_by_id(
        self,
        orderId: int,
    ) -> Order:
        """For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generate exceptions"""
        ...


    def place_order(
        self,
        order: Order,
    ) -> Order:
        """"""
        ...
