import connexion
import six

from openapi_server.models.client import Client  # noqa: E501
from openapi_server import util


def test_classname(client):  # noqa: E501
    """To test class name in snake case

    To test class name in snake case # noqa: E501

    :param client: client model
    :type client: dict | bytes

    :rtype: Client
    """
    if connexion.request.is_json:
        client = Client.from_dict(connexion.request.get_json())  # noqa: E501
    return 'do some magic!'
