import typing

from petstore_api.apis.tags import TagValues
from petstore_api.apis.tags.pet_api import PetApi
from petstore_api.apis.tags.store_api import StoreApi
from petstore_api.apis.tags.user_api import UserApi
from petstore_api.apis.tags.another_fake_api import AnotherFakeApi
from petstore_api.apis.tags.default_api import DefaultApi
from petstore_api.apis.tags.fake_api import FakeApi
from petstore_api.apis.tags.fake_classname_tags123_api import FakeClassnameTags123Api

TagToApi = typing.TypedDict(
    'TagToApi',
    {
        TagValues.PET: PetApi,
        TagValues.STORE: StoreApi,
        TagValues.USER: UserApi,
        TagValues.ANOTHERFAKE: AnotherFakeApi,
        TagValues.DEFAULT: DefaultApi,
        TagValues.FAKE: FakeApi,
        TagValues.FAKE_CLASSNAME_TAGS_123: FakeClassnameTags123Api,
    }
)

tag_to_api = TagToApi(
    {
        TagValues.PET: PetApi,
        TagValues.STORE: StoreApi,
        TagValues.USER: UserApi,
        TagValues.ANOTHERFAKE: AnotherFakeApi,
        TagValues.DEFAULT: DefaultApi,
        TagValues.FAKE: FakeApi,
        TagValues.FAKE_CLASSNAME_TAGS_123: FakeClassnameTags123Api,
    }
)
