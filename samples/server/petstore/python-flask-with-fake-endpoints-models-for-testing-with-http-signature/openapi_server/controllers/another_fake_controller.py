import connexion
import six

from openapi_server.models.client import Client  # noqa: E501
from openapi_server import util


def call123_test_special_tags(client):  # noqa: E501
    """To test special tags

    To test special tags and operation ID starting with number # noqa: E501

    :param client: client model
    :type client: dict | bytes

    :rtype: Client
    """
    if connexion.request.is_json:
        client = Client.from_dict(connexion.request.get_json())  # noqa: E501
    return 'do some magic!'
