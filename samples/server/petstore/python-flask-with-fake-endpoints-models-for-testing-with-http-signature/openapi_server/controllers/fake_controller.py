import connexion
import six

from openapi_server.models.client import Client  # noqa: E501
from openapi_server.models.file_schema_test_class import FileSchemaTestClass  # noqa: E501
from openapi_server.models.health_check_result import HealthCheckResult  # noqa: E501
from openapi_server.models.outer_composite import OuterComposite  # noqa: E501
from openapi_server.models.outer_object_with_enum_property import OuterObjectWithEnumProperty  # noqa: E501
from openapi_server.models.pet import Pet  # noqa: E501
from openapi_server.models.user import User  # noqa: E501
from openapi_server import util


def fake_health_get():  # noqa: E501
    """Health check endpoint

     # noqa: E501


    :rtype: HealthCheckResult
    """
    return 'do some magic!'


def fake_http_signature_test(pet, query_1=None, header_1=None):  # noqa: E501
    """test http signature authentication

     # noqa: E501

    :param pet: Pet object that needs to be added to the store
    :type pet: dict | bytes
    :param query_1: query parameter
    :type query_1: str
    :param header_1: header parameter
    :type header_1: str

    :rtype: None
    """
    if connexion.request.is_json:
        pet = Pet.from_dict(connexion.request.get_json())  # noqa: E501
    return 'do some magic!'


def fake_outer_boolean_serialize(body=None):  # noqa: E501
    """fake_outer_boolean_serialize

    Test serialization of outer boolean types # noqa: E501

    :param body: Input boolean as post body
    :type body: bool

    :rtype: bool
    """
    return 'do some magic!'


def fake_outer_composite_serialize(outer_composite=None):  # noqa: E501
    """fake_outer_composite_serialize

    Test serialization of object with outer number type # noqa: E501

    :param outer_composite: Input composite as post body
    :type outer_composite: dict | bytes

    :rtype: OuterComposite
    """
    if connexion.request.is_json:
        outer_composite = OuterComposite.from_dict(connexion.request.get_json())  # noqa: E501
    return 'do some magic!'


def fake_outer_number_serialize(body=None):  # noqa: E501
    """fake_outer_number_serialize

    Test serialization of outer number types # noqa: E501

    :param body: Input number as post body
    :type body: 

    :rtype: float
    """
    return 'do some magic!'


def fake_outer_string_serialize(body=None):  # noqa: E501
    """fake_outer_string_serialize

    Test serialization of outer string types # noqa: E501

    :param body: Input string as post body
    :type body: str

    :rtype: str
    """
    return 'do some magic!'


def fake_property_enum_integer_serialize(outer_object_with_enum_property):  # noqa: E501
    """fake_property_enum_integer_serialize

    Test serialization of enum (int) properties with examples # noqa: E501

    :param outer_object_with_enum_property: Input enum (int) as post body
    :type outer_object_with_enum_property: dict | bytes

    :rtype: OuterObjectWithEnumProperty
    """
    if connexion.request.is_json:
        outer_object_with_enum_property = OuterObjectWithEnumProperty.from_dict(connexion.request.get_json())  # noqa: E501
    return 'do some magic!'


def test_body_with_binary(body):  # noqa: E501
    """test_body_with_binary

    For this test, the body has to be a binary file. # noqa: E501

    :param body: image to upload
    :type body: str

    :rtype: None
    """
    return 'do some magic!'


def test_body_with_file_schema(file_schema_test_class):  # noqa: E501
    """test_body_with_file_schema

    For this test, the body for this request must reference a schema named &#x60;File&#x60;. # noqa: E501

    :param file_schema_test_class: 
    :type file_schema_test_class: dict | bytes

    :rtype: None
    """
    if connexion.request.is_json:
        file_schema_test_class = FileSchemaTestClass.from_dict(connexion.request.get_json())  # noqa: E501
    return 'do some magic!'


def test_body_with_query_params(query, user):  # noqa: E501
    """test_body_with_query_params

     # noqa: E501

    :param query: 
    :type query: str
    :param user: 
    :type user: dict | bytes

    :rtype: None
    """
    if connexion.request.is_json:
        user = User.from_dict(connexion.request.get_json())  # noqa: E501
    return 'do some magic!'


def test_client_model(client):  # noqa: E501
    """To test \&quot;client\&quot; model

    To test \&quot;client\&quot; model # noqa: E501

    :param client: client model
    :type client: dict | bytes

    :rtype: Client
    """
    if connexion.request.is_json:
        client = Client.from_dict(connexion.request.get_json())  # noqa: E501
    return 'do some magic!'


def test_endpoint_parameters(number, double, pattern_without_delimiter, byte, integer=None, int32=None, int64=None, float=None, string=None, binary=None, date=None, date_time=None, password=None, param_callback=None):  # noqa: E501
    """Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

    Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트  # noqa: E501

    :param number: None
    :type number: 
    :param double: None
    :type double: float
    :param pattern_without_delimiter: None
    :type pattern_without_delimiter: str
    :param byte: None
    :type byte: str
    :param integer: None
    :type integer: int
    :param int32: None
    :type int32: int
    :param int64: None
    :type int64: int
    :param float: None
    :type float: float
    :param string: None
    :type string: str
    :param binary: None
    :type binary: str
    :param date: None
    :type date: str
    :param date_time: None
    :type date_time: str
    :param password: None
    :type password: str
    :param param_callback: None
    :type param_callback: str

    :rtype: None
    """
    date = util.deserialize_date(date)
    date_time = util.deserialize_datetime(date_time)
    return 'do some magic!'


def test_enum_parameters(enum_header_string_array=None, enum_header_string=None, enum_query_string_array=None, enum_query_string=None, enum_query_integer=None, enum_query_double=None, enum_form_string_array=None, enum_form_string=None):  # noqa: E501
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
    return 'do some magic!'


def test_group_parameters(required_string_group, required_boolean_group, required_int64_group, string_group=None, boolean_group=None, int64_group=None):  # noqa: E501
    """Fake endpoint to test group parameters (optional)

    Fake endpoint to test group parameters (optional) # noqa: E501

    :param required_string_group: Required String in group parameters
    :type required_string_group: int
    :param required_boolean_group: Required Boolean in group parameters
    :type required_boolean_group: bool
    :param required_int64_group: Required Integer in group parameters
    :type required_int64_group: int
    :param string_group: String in group parameters
    :type string_group: int
    :param boolean_group: Boolean in group parameters
    :type boolean_group: bool
    :param int64_group: Integer in group parameters
    :type int64_group: int

    :rtype: None
    """
    return 'do some magic!'


def test_inline_additional_properties(request_body):  # noqa: E501
    """test inline additionalProperties

     # noqa: E501

    :param request_body: request body
    :type request_body: Dict[str, str]

    :rtype: None
    """
    return 'do some magic!'


def test_json_form_data(param, param2):  # noqa: E501
    """test json serialization of form data

     # noqa: E501

    :param param: field1
    :type param: str
    :param param2: field2
    :type param2: str

    :rtype: None
    """
    return 'do some magic!'


def test_query_parameter_collection_format(pipe, ioutil, http, url, context, allow_empty, language=None):  # noqa: E501
    """test_query_parameter_collection_format

    To test the collection format in query parameters # noqa: E501

    :param pipe: 
    :type pipe: List[str]
    :param ioutil: 
    :type ioutil: List[str]
    :param http: 
    :type http: List[str]
    :param url: 
    :type url: List[str]
    :param context: 
    :type context: List[str]
    :param allow_empty: 
    :type allow_empty: str
    :param language: 
    :type language: Dict[str, str]

    :rtype: None
    """
    return 'do some magic!'
