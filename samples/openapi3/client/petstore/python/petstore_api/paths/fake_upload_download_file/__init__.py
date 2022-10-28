# do not import all endpoints into this module because that uses a lot of memory and stack frames
# if you need the ability to import all endpoints from this module, import them with
# from petstore_api.paths.fake_upload_download_file import Api

from petstore_api.paths import PathValues

path = PathValues.FAKE_UPLOAD_DOWNLOAD_FILE