# do not import all endpoints into this module because that uses a lot of memory and stack frames
# if you need the ability to import all endpoints from this module, import them with
# from petstore_api.paths.pet_pet_id_3 import Api

from petstore_api.paths import PathValues

path = PathValues.PET_PET_ID