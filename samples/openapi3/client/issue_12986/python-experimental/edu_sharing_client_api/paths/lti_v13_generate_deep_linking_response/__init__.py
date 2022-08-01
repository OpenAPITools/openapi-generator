# do not import all endpoints into this module because that uses a lot of memory and stack frames
# if you need the ability to import all endpoints from this module, import them with
# from edu_sharing_client_api.paths.lti_v13_generate_deep_linking_response import Api

from edu_sharing_client_api.paths import PathValues

path = PathValues.LTI_V13_GENERATE_DEEP_LINKING_RESPONSE