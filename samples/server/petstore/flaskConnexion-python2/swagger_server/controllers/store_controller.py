import connexion
from swagger_server.models.order import Order
from datetime import date, datetime
from typing import List, Dict
from six import iteritems
from ..util import deserialize_date, deserialize_datetime


def delete_order(orderId):
    """
    Delete purchase order by ID
    For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
    :param orderId: ID of the order that needs to be deleted
    :type orderId: str

    :rtype: None
    """
    return 'do some magic!'


def get_inventory():
    """
    Returns pet inventories by status
    Returns a map of status codes to quantities

    :rtype: Dict[str, int]
    """
    return 'do some magic!'


def get_order_by_id(orderId):
    """
    Find purchase order by ID
    For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
    :param orderId: ID of pet that needs to be fetched
    :type orderId: int

    :rtype: Order
    """
    return 'do some magic!'


def place_order(body):
    """
    Place an order for a pet
    
    :param body: order placed for purchasing the pet
    :type body: dict | bytes

    :rtype: Order
    """
    if connexion.request.is_json:
        body = Order.from_dict(connexion.request.get_json())
    return 'do some magic!'
