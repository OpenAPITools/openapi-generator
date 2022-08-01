# do not import all endpoints into this module because that uses a lot of memory and stack frames
# if you need the ability to import all endpoints from this module, import them with
# from petstore_api.apis.tag_to_api import tag_to_api

import enum


class TagValues(str, enum.Enum):
    PET = "pet"
    STORE = "store"
    USER = "user"
    ANOTHERFAKE = "$another-fake?"
    DEFAULT = "default"
    FAKE = "fake"
    FAKE_CLASSNAME_TAGS_123 = "fake_classname_tags 123#$%^"
