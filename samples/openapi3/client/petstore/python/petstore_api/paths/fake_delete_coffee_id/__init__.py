# do not import all endpoints into this module because that uses a lot of memory and stack frames
# if you need the ability to import all endpoints from this module, import them with
# from petstore_api.paths.fake_delete_coffee_id import Api

from petstore_api.paths import PathValues

path = PathValues.FAKE_DELETE_COFFEE_ID