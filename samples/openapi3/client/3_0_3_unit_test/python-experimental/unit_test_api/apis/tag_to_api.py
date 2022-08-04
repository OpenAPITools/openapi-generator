import typing

from unit_test_api.apis.tags import TagValues
from unit_test_api.apis.tags.operation_request_body_api import OperationRequestBodyApi
from unit_test_api.apis.tags.path_post_api import PathPostApi
from unit_test_api.apis.tags.content_type_json_api import ContentTypeJsonApi
from unit_test_api.apis.tags.response_content_content_type_schema_api import ResponseContentContentTypeSchemaApi
from unit_test_api.apis.tags.additional_properties_api import AdditionalPropertiesApi
from unit_test_api.apis.tags.all_of_api import AllOfApi
from unit_test_api.apis.tags.any_of_api import AnyOfApi
from unit_test_api.apis.tags.default_api import DefaultApi
from unit_test_api.apis.tags.enum_api import EnumApi
from unit_test_api.apis.tags.format_api import FormatApi
from unit_test_api.apis.tags.items_api import ItemsApi
from unit_test_api.apis.tags.maximum_api import MaximumApi
from unit_test_api.apis.tags.max_items_api import MaxItemsApi
from unit_test_api.apis.tags.max_length_api import MaxLengthApi
from unit_test_api.apis.tags.max_properties_api import MaxPropertiesApi
from unit_test_api.apis.tags.minimum_api import MinimumApi
from unit_test_api.apis.tags.min_items_api import MinItemsApi
from unit_test_api.apis.tags.min_length_api import MinLengthApi
from unit_test_api.apis.tags.min_properties_api import MinPropertiesApi
from unit_test_api.apis.tags.multiple_of_api import MultipleOfApi
from unit_test_api.apis.tags.model_not_api import ModelNotApi
from unit_test_api.apis.tags.one_of_api import OneOfApi
from unit_test_api.apis.tags.pattern_api import PatternApi
from unit_test_api.apis.tags.properties_api import PropertiesApi
from unit_test_api.apis.tags.ref_api import RefApi
from unit_test_api.apis.tags.required_api import RequiredApi
from unit_test_api.apis.tags.type_api import TypeApi
from unit_test_api.apis.tags.unique_items_api import UniqueItemsApi

TagToApi = typing.TypedDict(
    'TagToApi',
    {
        TagValues.OPERATION_REQUEST_BODY: OperationRequestBodyApi,
        TagValues.PATH_POST: PathPostApi,
        TagValues.CONTENT_TYPE_JSON: ContentTypeJsonApi,
        TagValues.RESPONSE_CONTENT_CONTENT_TYPE_SCHEMA: ResponseContentContentTypeSchemaApi,
        TagValues.ADDITIONAL_PROPERTIES: AdditionalPropertiesApi,
        TagValues.ALL_OF: AllOfApi,
        TagValues.ANY_OF: AnyOfApi,
        TagValues.DEFAULT: DefaultApi,
        TagValues.ENUM: EnumApi,
        TagValues.FORMAT: FormatApi,
        TagValues.ITEMS: ItemsApi,
        TagValues.MAXIMUM: MaximumApi,
        TagValues.MAX_ITEMS: MaxItemsApi,
        TagValues.MAX_LENGTH: MaxLengthApi,
        TagValues.MAX_PROPERTIES: MaxPropertiesApi,
        TagValues.MINIMUM: MinimumApi,
        TagValues.MIN_ITEMS: MinItemsApi,
        TagValues.MIN_LENGTH: MinLengthApi,
        TagValues.MIN_PROPERTIES: MinPropertiesApi,
        TagValues.MULTIPLE_OF: MultipleOfApi,
        TagValues.NOT: ModelNotApi,
        TagValues.ONE_OF: OneOfApi,
        TagValues.PATTERN: PatternApi,
        TagValues.PROPERTIES: PropertiesApi,
        TagValues.REF: RefApi,
        TagValues.REQUIRED: RequiredApi,
        TagValues.TYPE: TypeApi,
        TagValues.UNIQUE_ITEMS: UniqueItemsApi,
    }
)

tag_to_api = TagToApi(
    {
        TagValues.OPERATION_REQUEST_BODY: OperationRequestBodyApi,
        TagValues.PATH_POST: PathPostApi,
        TagValues.CONTENT_TYPE_JSON: ContentTypeJsonApi,
        TagValues.RESPONSE_CONTENT_CONTENT_TYPE_SCHEMA: ResponseContentContentTypeSchemaApi,
        TagValues.ADDITIONAL_PROPERTIES: AdditionalPropertiesApi,
        TagValues.ALL_OF: AllOfApi,
        TagValues.ANY_OF: AnyOfApi,
        TagValues.DEFAULT: DefaultApi,
        TagValues.ENUM: EnumApi,
        TagValues.FORMAT: FormatApi,
        TagValues.ITEMS: ItemsApi,
        TagValues.MAXIMUM: MaximumApi,
        TagValues.MAX_ITEMS: MaxItemsApi,
        TagValues.MAX_LENGTH: MaxLengthApi,
        TagValues.MAX_PROPERTIES: MaxPropertiesApi,
        TagValues.MINIMUM: MinimumApi,
        TagValues.MIN_ITEMS: MinItemsApi,
        TagValues.MIN_LENGTH: MinLengthApi,
        TagValues.MIN_PROPERTIES: MinPropertiesApi,
        TagValues.MULTIPLE_OF: MultipleOfApi,
        TagValues.NOT: ModelNotApi,
        TagValues.ONE_OF: OneOfApi,
        TagValues.PATTERN: PatternApi,
        TagValues.PROPERTIES: PropertiesApi,
        TagValues.REF: RefApi,
        TagValues.REQUIRED: RequiredApi,
        TagValues.TYPE: TypeApi,
        TagValues.UNIQUE_ITEMS: UniqueItemsApi,
    }
)
