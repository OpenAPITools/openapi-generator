import connexion
import six

from openapi_server.models.enum_cornucopia import EnumCornucopia  # noqa: E501
from openapi_server import util


def enum_object():  # noqa: E501
    """enum_object

    Test serialization of enum (int) properties with examples # noqa: E501

    :param enum_cornucopia: 
    :type enum_cornucopia: dict | bytes

    :rtype: EnumCornucopia
    """
    
    if connexion.request.is_json:
        enum_cornucopia = EnumCornucopia.from_dict(connexion.request.get_json())  # noqa: E501
        print("JSON: ", enum_cornucopia)
    return 'do some magic!'


def enums(enum_header_string_array=None, enum_header_string=None, enum_query_string_array=None, enum_query_string=None, enum_query_integer=None, enum_query_double=None, enum_form_string_array=None, enum_form_string=None):  # noqa: E501
    """To test enum parameters

    To test enum parameters # noqa: E501

    :param enum_header_string_array: Header parameter enum test (string array)
    :type enum_header_string_array: List[str]
    :param enum_header_string: Header parameter enum test (string)
    :type enum_header_string: str
    :param enum_query_string_array: Query parameter enum test (string array)
    :type enum_query_string_array: List[str]
    :param enum_query_string: Query parameter enum test (string)
    :type enum_query_string: str
    :param enum_query_integer: Query parameter enum test (double)
    :type enum_query_integer: int
    :param enum_query_double: Query parameter enum test (double)
    :type enum_query_double: float
    :param enum_form_string_array: Form parameter enum test (string array)
    :type enum_form_string_array: List[str]
    :param enum_form_string: Form parameter enum test (string)
    :type enum_form_string: str

    :rtype: None
    """
    print("PARAM: enum_header_string_array=", str(enum_header_string_array))
    print("PARAM: enum_header_string=", str(enum_header_string))
    print("PARAM: enum_query_string_array=", str(enum_query_string_array))
    print("PARAM: enum_query_string=", str(enum_query_string))
    print("PARAM: enum_query_integer=", str(enum_query_integer))
    print("PARAM: enum_query_double=", str(enum_query_double))
    print("PARAM: enum_form_string_array=", str(enum_form_string_array))
    print("PARAM: enum_form_string=", str(enum_form_string))
    return 'do some magic!'
