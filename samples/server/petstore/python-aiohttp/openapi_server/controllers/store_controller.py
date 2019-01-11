from typing import List, Dict
from aiohttp import web

from openapi_server.models.order import Order
from openapi_server import util


async def delete_order(request: web.Request, order_id) -> web.Response:
    """Delete purchase order by ID

    For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors

    :param order_id: ID of the order that needs to be deleted
    :type order_id: str

    """
    return web.Response(status=200)


async def get_inventory(request: web.Request, ) -> web.Response:
    """Returns pet inventories by status

    Returns a map of status codes to quantities


    """
    return web.Response(status=200)


async def get_order_by_id(request: web.Request, order_id) -> web.Response:
    """Find purchase order by ID

    For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions

    :param order_id: ID of pet that needs to be fetched
    :type order_id: int

    """
    return web.Response(status=200)


async def place_order(request: web.Request, body) -> web.Response:
    """Place an order for a pet

    

    :param body: order placed for purchasing the pet
    :type body: dict | bytes

    """
    body = Order.from_dict(body)
    return web.Response(status=200)
