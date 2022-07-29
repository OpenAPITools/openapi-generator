import typing

from unit_test_api.apis.tags import TagValues
from unit_test_api.apis.tags.operation_request_body_api import OperationRequestBodyApi
from unit_test_api.apis.tags.path_post_api import PathPostApi
from unit_test_api.apis.tags.content_type_json_api import ContentTypeJsonApi
from unit_test_api.apis.tags.response_content_content_type_schema_api import ResponseContentContentTypeSchemaApi

TagToApi = typing.TypedDict(
    'TagToApi',
    {
        TagValues.OPERATION_REQUEST_BODY: OperationRequestBodyApi,
        TagValues.PATH_POST: PathPostApi,
        TagValues.CONTENT_TYPE_JSON: ContentTypeJsonApi,
        TagValues.RESPONSE_CONTENT_CONTENT_TYPE_SCHEMA: ResponseContentContentTypeSchemaApi,
    }
)

tag_to_api = TagToApi(
    {
        TagValues.OPERATION_REQUEST_BODY: OperationRequestBodyApi,
        TagValues.PATH_POST: PathPostApi,
        TagValues.CONTENT_TYPE_JSON: ContentTypeJsonApi,
        TagValues.RESPONSE_CONTENT_CONTENT_TYPE_SCHEMA: ResponseContentContentTypeSchemaApi,
    }
)
