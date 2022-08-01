# do not import all endpoints into this module because that uses a lot of memory and stack frames
# if you need the ability to import all endpoints from this module, import them with
# from unit_test_api.apis.tag_to_api import tag_to_api

import enum


class TagValues(str, enum.Enum):
    OPERATION_REQUEST_BODY = "operation.requestBody"
    PATH_POST = "path.post"
    CONTENT_TYPE_JSON = "contentType_json"
    RESPONSE_CONTENT_CONTENT_TYPE_SCHEMA = "response.content.contentType.schema"
