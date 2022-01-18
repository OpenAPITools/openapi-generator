import connexion
import six

from openapi_server.models.inline_response_default import InlineResponseDefault  # noqa: E501
from openapi_server import util


def foo_get():  # noqa: E501
    """foo_get

     # noqa: E501


    :rtype: InlineResponseDefault
    """
    return 'do some magic!'
