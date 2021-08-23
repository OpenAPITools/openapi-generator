from uplink import (
    Consumer,
    get,
    Path,
    Query,
    Body,
    post,
    get,
    patch,
    put,
    delete,
    Header,
    returns,
    json,
)

from typing import Dict, List  # noqa: F401

from openapi_client.model.order import Order


class StoreApi(Consumer):
    @delete("/store/order/{orderId}")
    def delete_order(self, *, orderId: str):
        """Delete purchase order by ID"""

    @returns.json
    @get("/store/inventory")
    def get_inventory(self) -> Dict[str, int]:
        """Returns pet inventories by status"""

    @returns.json
    @get("/store/order/{orderId}")
    def get_order_by_id(self, *, orderId: int) -> Order:
        """Find purchase order by ID"""

    @returns.json
    @post("/store/order")
    def place_order(self, *, body: Body(type=Order)) -> Order:
        """Place an order for a pet"""

