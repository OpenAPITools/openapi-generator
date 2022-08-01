# do not import all endpoints into this module because that uses a lot of memory and stack frames
# if you need the ability to import all endpoints from this module, import them with
# from edu_sharing_client_api.paths.bulk_v1_find import Api

from edu_sharing_client_api.paths import PathValues

path = PathValues.BULK_V1_FIND