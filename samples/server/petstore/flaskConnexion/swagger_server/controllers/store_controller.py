import connexion
import six

from swagger_server.models.order import Order  # noqa: E501
from swagger_server import util


def delete_order(orderId):  # noqa: E501
    """Delete purchase order by ID

    For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors # noqa: E501

    :param orderId: ID of the order that needs to be deleted
    :type orderId: str

    :rtype: None
    """
    return 'do some magic!'


def get_inventory():  # noqa: E501
    """Returns pet inventories by status

    Returns a map of status codes to quantities # noqa: E501


    :rtype: Dict[str, int]
    """
    return 'do some magic!'


def get_order_by_id(orderId):  # noqa: E501
    """Find purchase order by ID

    For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions # noqa: E501

    :param orderId: ID of pet that needs to be fetched
    :type orderId: int

    :rtype: Order
    """
    return 'do some magic!'


def place_order(body):  # noqa: E501
    """Place an order for a pet

     # noqa: E501

    :param body: order placed for purchasing the pet
    :type body: dict | bytes

    :rtype: Order
    """
    if connexion.request.is_json:
        body = Order.from_dict(connexion.request.get_json())  # noqa: E501
    return 'do some magic!'
