import typing_extensions

from petstore_api.paths import PathValues
from petstore_api.apis.paths.foo import Foo
from petstore_api.apis.paths.pet import Pet
from petstore_api.apis.paths.pet_find_by_status import PetFindByStatus
from petstore_api.apis.paths.pet_find_by_tags import PetFindByTags
from petstore_api.apis.paths.pet_pet_id import PetPetId
from petstore_api.apis.paths.pet_pet_id_upload_image import PetPetIdUploadImage
from petstore_api.apis.paths.store_inventory import StoreInventory
from petstore_api.apis.paths.store_order import StoreOrder
from petstore_api.apis.paths.store_order_order_id import StoreOrderOrderId
from petstore_api.apis.paths.user import User
from petstore_api.apis.paths.user_create_with_array import UserCreateWithArray
from petstore_api.apis.paths.user_create_with_list import UserCreateWithList
from petstore_api.apis.paths.user_login import UserLogin
from petstore_api.apis.paths.user_logout import UserLogout
from petstore_api.apis.paths.user_username import UserUsername
from petstore_api.apis.paths.fake_classname_test import FakeClassnameTest
from petstore_api.apis.paths.fake import Fake
from petstore_api.apis.paths.fake_refs_number import FakeRefsNumber
from petstore_api.apis.paths.fake_refs_mammal import FakeRefsMammal
from petstore_api.apis.paths.fake_refs_string import FakeRefsString
from petstore_api.apis.paths.fake_refs_boolean import FakeRefsBoolean
from petstore_api.apis.paths.fake_refs_arraymodel import FakeRefsArraymodel
from petstore_api.apis.paths.fake_refs_composed_one_of_number_with_validations import FakeRefsComposedOneOfNumberWithValidations
from petstore_api.apis.paths.fake_refs_object_model_with_ref_props import FakeRefsObjectModelWithRefProps
from petstore_api.apis.paths.fake_refs_enum import FakeRefsEnum
from petstore_api.apis.paths.fake_refs_array_of_enums import FakeRefsArrayOfEnums
from petstore_api.apis.paths.fake_additional_properties_with_array_of_enums import FakeAdditionalPropertiesWithArrayOfEnums
from petstore_api.apis.paths.fake_json_form_data import FakeJsonFormData
from petstore_api.apis.paths.fake_inline_additional_properties import FakeInlineAdditionalProperties
from petstore_api.apis.paths.fake_body_with_query_params import FakeBodyWithQueryParams
from petstore_api.apis.paths.another_fake_dummy import AnotherFakeDummy
from petstore_api.apis.paths.fake_body_with_file_schema import FakeBodyWithFileSchema
from petstore_api.apis.paths.fake_case_sensitive_params import FakeCaseSensitiveParams
from petstore_api.apis.paths.fake_test_query_parameters import FakeTestQueryParameters
from petstore_api.apis.paths.fake_pet_id_upload_image_with_required_file import FakePetIdUploadImageWithRequiredFile
from petstore_api.apis.paths.fake_parameter_collisions_1_a_b_ab_self_a_b_ import FakeParameterCollisions1ABAbSelfAB
from petstore_api.apis.paths.fake_upload_file import FakeUploadFile
from petstore_api.apis.paths.fake_upload_files import FakeUploadFiles
from petstore_api.apis.paths.fake_upload_download_file import FakeUploadDownloadFile
from petstore_api.apis.paths.fake_health import FakeHealth
from petstore_api.apis.paths.fake_inline_composition_ import FakeInlineComposition
from petstore_api.apis.paths.fake_obj_in_query import FakeObjInQuery
from petstore_api.apis.paths.fake_ref_obj_in_query import FakeRefObjInQuery
from petstore_api.apis.paths.fake_json_with_charset import FakeJsonWithCharset
from petstore_api.apis.paths.fake_response_without_schema import FakeResponseWithoutSchema
from petstore_api.apis.paths.fake_json_patch import FakeJsonPatch
from petstore_api.apis.paths.fake_delete_coffee_id import FakeDeleteCoffeeId
from petstore_api.apis.paths.fake_query_param_with_json_content_type import FakeQueryParamWithJsonContentType

PathToApi = typing_extensions.TypedDict(
    'PathToApi',
    {
        PathValues.FOO: Foo,
        PathValues.PET: Pet,
        PathValues.PET_FIND_BY_STATUS: PetFindByStatus,
        PathValues.PET_FIND_BY_TAGS: PetFindByTags,
        PathValues.PET_PET_ID: PetPetId,
        PathValues.PET_PET_ID_UPLOAD_IMAGE: PetPetIdUploadImage,
        PathValues.STORE_INVENTORY: StoreInventory,
        PathValues.STORE_ORDER: StoreOrder,
        PathValues.STORE_ORDER_ORDER_ID: StoreOrderOrderId,
        PathValues.USER: User,
        PathValues.USER_CREATE_WITH_ARRAY: UserCreateWithArray,
        PathValues.USER_CREATE_WITH_LIST: UserCreateWithList,
        PathValues.USER_LOGIN: UserLogin,
        PathValues.USER_LOGOUT: UserLogout,
        PathValues.USER_USERNAME: UserUsername,
        PathValues.FAKE_CLASSNAME_TEST: FakeClassnameTest,
        PathValues.FAKE: Fake,
        PathValues.FAKE_REFS_NUMBER: FakeRefsNumber,
        PathValues.FAKE_REFS_MAMMAL: FakeRefsMammal,
        PathValues.FAKE_REFS_STRING: FakeRefsString,
        PathValues.FAKE_REFS_BOOLEAN: FakeRefsBoolean,
        PathValues.FAKE_REFS_ARRAYMODEL: FakeRefsArraymodel,
        PathValues.FAKE_REFS_COMPOSED_ONE_OF_NUMBER_WITH_VALIDATIONS: FakeRefsComposedOneOfNumberWithValidations,
        PathValues.FAKE_REFS_OBJECT_MODEL_WITH_REF_PROPS: FakeRefsObjectModelWithRefProps,
        PathValues.FAKE_REFS_ENUM: FakeRefsEnum,
        PathValues.FAKE_REFS_ARRAYOFENUMS: FakeRefsArrayOfEnums,
        PathValues.FAKE_ADDITIONALPROPERTIESWITHARRAYOFENUMS: FakeAdditionalPropertiesWithArrayOfEnums,
        PathValues.FAKE_JSON_FORM_DATA: FakeJsonFormData,
        PathValues.FAKE_INLINEADDITIONAL_PROPERTIES: FakeInlineAdditionalProperties,
        PathValues.FAKE_BODYWITHQUERYPARAMS: FakeBodyWithQueryParams,
        PathValues.ANOTHERFAKE_DUMMY: AnotherFakeDummy,
        PathValues.FAKE_BODYWITHFILESCHEMA: FakeBodyWithFileSchema,
        PathValues.FAKE_CASESENSITIVEPARAMS: FakeCaseSensitiveParams,
        PathValues.FAKE_TESTQUERYPARAMETERS: FakeTestQueryParameters,
        PathValues.FAKE_PET_ID_UPLOAD_IMAGE_WITH_REQUIRED_FILE: FakePetIdUploadImageWithRequiredFile,
        PathValues.FAKE_PARAMETER_COLLISIONS_1_A_B_AB_SELF_AB_: FakeParameterCollisions1ABAbSelfAB,
        PathValues.FAKE_UPLOAD_FILE: FakeUploadFile,
        PathValues.FAKE_UPLOAD_FILES: FakeUploadFiles,
        PathValues.FAKE_UPLOAD_DOWNLOAD_FILE: FakeUploadDownloadFile,
        PathValues.FAKE_HEALTH: FakeHealth,
        PathValues.FAKE_INLINE_COMPOSITION_: FakeInlineComposition,
        PathValues.FAKE_OBJ_IN_QUERY: FakeObjInQuery,
        PathValues.FAKE_REF_OBJ_IN_QUERY: FakeRefObjInQuery,
        PathValues.FAKE_JSON_WITH_CHARSET: FakeJsonWithCharset,
        PathValues.FAKE_RESPONSE_WITHOUT_SCHEMA: FakeResponseWithoutSchema,
        PathValues.FAKE_JSON_PATCH: FakeJsonPatch,
        PathValues.FAKE_DELETE_COFFEE_ID: FakeDeleteCoffeeId,
        PathValues.FAKE_QUERY_PARAM_WITH_JSON_CONTENT_TYPE: FakeQueryParamWithJsonContentType,
    }
)

path_to_api = PathToApi(
    {
        PathValues.FOO: Foo,
        PathValues.PET: Pet,
        PathValues.PET_FIND_BY_STATUS: PetFindByStatus,
        PathValues.PET_FIND_BY_TAGS: PetFindByTags,
        PathValues.PET_PET_ID: PetPetId,
        PathValues.PET_PET_ID_UPLOAD_IMAGE: PetPetIdUploadImage,
        PathValues.STORE_INVENTORY: StoreInventory,
        PathValues.STORE_ORDER: StoreOrder,
        PathValues.STORE_ORDER_ORDER_ID: StoreOrderOrderId,
        PathValues.USER: User,
        PathValues.USER_CREATE_WITH_ARRAY: UserCreateWithArray,
        PathValues.USER_CREATE_WITH_LIST: UserCreateWithList,
        PathValues.USER_LOGIN: UserLogin,
        PathValues.USER_LOGOUT: UserLogout,
        PathValues.USER_USERNAME: UserUsername,
        PathValues.FAKE_CLASSNAME_TEST: FakeClassnameTest,
        PathValues.FAKE: Fake,
        PathValues.FAKE_REFS_NUMBER: FakeRefsNumber,
        PathValues.FAKE_REFS_MAMMAL: FakeRefsMammal,
        PathValues.FAKE_REFS_STRING: FakeRefsString,
        PathValues.FAKE_REFS_BOOLEAN: FakeRefsBoolean,
        PathValues.FAKE_REFS_ARRAYMODEL: FakeRefsArraymodel,
        PathValues.FAKE_REFS_COMPOSED_ONE_OF_NUMBER_WITH_VALIDATIONS: FakeRefsComposedOneOfNumberWithValidations,
        PathValues.FAKE_REFS_OBJECT_MODEL_WITH_REF_PROPS: FakeRefsObjectModelWithRefProps,
        PathValues.FAKE_REFS_ENUM: FakeRefsEnum,
        PathValues.FAKE_REFS_ARRAYOFENUMS: FakeRefsArrayOfEnums,
        PathValues.FAKE_ADDITIONALPROPERTIESWITHARRAYOFENUMS: FakeAdditionalPropertiesWithArrayOfEnums,
        PathValues.FAKE_JSON_FORM_DATA: FakeJsonFormData,
        PathValues.FAKE_INLINEADDITIONAL_PROPERTIES: FakeInlineAdditionalProperties,
        PathValues.FAKE_BODYWITHQUERYPARAMS: FakeBodyWithQueryParams,
        PathValues.ANOTHERFAKE_DUMMY: AnotherFakeDummy,
        PathValues.FAKE_BODYWITHFILESCHEMA: FakeBodyWithFileSchema,
        PathValues.FAKE_CASESENSITIVEPARAMS: FakeCaseSensitiveParams,
        PathValues.FAKE_TESTQUERYPARAMETERS: FakeTestQueryParameters,
        PathValues.FAKE_PET_ID_UPLOAD_IMAGE_WITH_REQUIRED_FILE: FakePetIdUploadImageWithRequiredFile,
        PathValues.FAKE_PARAMETER_COLLISIONS_1_A_B_AB_SELF_AB_: FakeParameterCollisions1ABAbSelfAB,
        PathValues.FAKE_UPLOAD_FILE: FakeUploadFile,
        PathValues.FAKE_UPLOAD_FILES: FakeUploadFiles,
        PathValues.FAKE_UPLOAD_DOWNLOAD_FILE: FakeUploadDownloadFile,
        PathValues.FAKE_HEALTH: FakeHealth,
        PathValues.FAKE_INLINE_COMPOSITION_: FakeInlineComposition,
        PathValues.FAKE_OBJ_IN_QUERY: FakeObjInQuery,
        PathValues.FAKE_REF_OBJ_IN_QUERY: FakeRefObjInQuery,
        PathValues.FAKE_JSON_WITH_CHARSET: FakeJsonWithCharset,
        PathValues.FAKE_RESPONSE_WITHOUT_SCHEMA: FakeResponseWithoutSchema,
        PathValues.FAKE_JSON_PATCH: FakeJsonPatch,
        PathValues.FAKE_DELETE_COFFEE_ID: FakeDeleteCoffeeId,
        PathValues.FAKE_QUERY_PARAM_WITH_JSON_CONTENT_TYPE: FakeQueryParamWithJsonContentType,
    }
)
