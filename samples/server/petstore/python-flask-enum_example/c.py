import connexion
import six

from openapi_server.models.enum_cornucopia import EnumCornucopia  # noqa: E501
from openapi_server.models.outer_enum_integer import OuterEnumInteger  # noqa: E501
from openapi_server import util


def enum_get(enum_header_string_array=None, enum_header_string=None, enum_query_string_array=None, enum_query_string=None, enum_query_integer=None, enum_query_double=None, enum_form_string_array=None, enum_form_string=None):  # noqa: E501
    """To test enum parameters

    To test enum parameters # noqa: E501

    :param enum_header_string_array: Header parameter  - HTTP convention for headers is hyphen NOT underscore but both are legal so we&#39;ll use both here deliberately to highlight what the processing does
    :type enum_header_string_array: List[str]
    :param enum_header_string: Header parameter - HTTP convention for headers is hyphen NOT underscore but both are legal so we&#39;ll use both here deliberately to highlight what the processing does
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
    
    print("HeaderParam enum_header_string_array=",str(connexion.request.headers['enum_header_string_array']))
    
    
    print("HeaderParam enum_header_string=",str(connexion.request.headers['enum_header_string']))
    
    print("QueryParam enum_query_string_array=",str(enum_query_string_array))
    
    
    print("QueryParam enum_query_string=",str(enum_query_string))
    
    
    print("QueryParam enum_query_integer=",str(enum_query_integer))
    
    
    print("QueryParam enum_query_double=",str(enum_query_double))
    
    
    
    
    print("FormParam enum_form_string_array=",str(connexion.request.form['enum_form_string_array']))
    
    
    print("FormParam enum_form_string=",str(connexion.request.form['enum_form_string']))
    return 'do some magic!'


def enum_post_form(enum_int, enum_form_string_array=None, enum_form_string=None):  # noqa: E501
    try:
        """enum_post_form

        Test serialization of enum properties with examples # noqa: E501

        :param enum_int: Query outer enum parameter
        :type enum_int: dict | bytes
        :param enum_form_string_array: Form parameter enum test (string array)
        :type enum_form_string_array: List[str]
        :param enum_form_string: Form parameter enum test (string)
        :type enum_form_string: str

        :rtype: EnumCornucopia
        """
        print("QueryParam enum_int=",str(enum_int))
        
        
        
        
        print("FormParam enum_form_string_array=",str(connexion.request.form))
        print("FormParam enum_form_string_array=",type(connexion.request.form))
        print("1")
        print("FormParam enum_form_string=",str(enum_form_string))
        print("1")
        print("FormParam enum_form_string=",str(connexion.request.form['enum_form_string']))
        print("2")
        print("FormParam enum_form_string_array=",str(enum_form_string_array))
        print("2")
        print("FormParam enum_form_string_array=",str(connexion.request.form['enum_form_string_array']))
        return 'do some magic!'
    except Exception as ex:
        print("HANDLER EXCEPTION : ", ex)
        raise ex 


def enum_post_json(enum_int, body):  # noqa: E501
    """enum_post_json

    Test serialization of enum properties with examples # noqa: E501

    :param enum_int: Query outer enum parameter
    :type enum_int: dict | bytes
    :param enum_cornucopia: 
    :type enum_cornucopia: dict | bytes

    :rtype: EnumCornucopia
    """
    print("QueryParam enum_int=",str(enum_int))
    
    
    print("BodyParam body=", str(body))
    if connexion.request.is_json:
        print("Request JSON: ", connexion.request.get_json())
        enum_cornucopia = EnumCornucopia.from_dict(connexion.request.get_json())  # noqa: E501
        print("Decoded Request JSON: enum_cornucopia=", enum_cornucopia)
    return 'do some magic!'
