import connexion
import six
from typing import Dict
from typing import Tuple
from typing import Union

from openapi_server.models.order import Order  # noqa: E501
from openapi_server import util


def delete_order(order_id):  # noqa: E501
    """Delete purchase order by ID

    For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors # noqa: E501

    :param order_id: ID of the order that needs to be deleted
    :type order_id: str

    :rtype: Union[None, Tuple[None, int], Tuple[None, int, Dict[str, str]]
    """
    return 'do some magic!'


def get_inventory():  # noqa: E501
    """Returns pet inventories by status

    Returns a map of status codes to quantities # noqa: E501


    :rtype: Union[Dict[str, int], Tuple[Dict[str, int], int], Tuple[Dict[str, int], int, Dict[str, str]]
    """
    return 'do some magic!'


def get_order_by_id(order_id):  # noqa: E501
    """Find purchase order by ID

    For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generate exceptions # noqa: E501

    :param order_id: ID of pet that needs to be fetched
    :type order_id: int

    :rtype: Union[Order, Tuple[Order, int], Tuple[Order, int, Dict[str, str]]
    """
    return 'do some magic!'


def place_order(body):  # noqa: E501
    """Place an order for a pet

     # noqa: E501

    :param body: order placed for purchasing the pet
    :type body: dict | bytes

    :rtype: Union[Order, Tuple[Order, int], Tuple[Order, int, Dict[str, str]]
    """
    if connexion.request.is_json:
        body = Order.from_dict(connexion.request.get_json())  # noqa: E501
    return 'do some magic!'
