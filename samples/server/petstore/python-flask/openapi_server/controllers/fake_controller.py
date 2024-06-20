import connexion
from typing import Dict
from typing import Tuple
from typing import Union

from openapi_server import util


def fake_query_param_default(has_default=None, no_default=None):  # noqa: E501
    """test query parameter default value

     # noqa: E501

    :param has_default: has default value
    :type has_default: str
    :param no_default: no default value
    :type no_default: str

    :rtype: Union[None, Tuple[None, int], Tuple[None, int, Dict[str, str]]
    """
    return 'do some magic!'
