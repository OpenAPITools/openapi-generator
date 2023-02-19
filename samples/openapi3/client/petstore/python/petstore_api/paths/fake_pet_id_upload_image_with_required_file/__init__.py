# do not import all endpoints into this module because that uses a lot of memory and stack frames
# if you need the ability to import all endpoints from this module, import them with
# from petstore_api.paths.fake_pet_id_upload_image_with_required_file import Api

from petstore_api.paths import PathValues

path = PathValues.FAKE_PET_ID_UPLOAD_IMAGE_WITH_REQUIRED_FILE