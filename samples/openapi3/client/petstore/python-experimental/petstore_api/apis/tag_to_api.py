import typing

from petstore_api.apis.tags import TagValues
from petstore_api.apis.tags.pet_api import PetApi
from petstore_api.apis.tags.store_api import StoreApi
from petstore_api.apis.tags.user_api import UserApi

TagToApi = typing.TypedDict(
    'TagToApi',
    {
        TagValues.PET: PetApi,
        TagValues.STORE: StoreApi,
        TagValues.USER: UserApi,
    }
)

tag_to_api = TagToApi(
    {
        TagValues.PET: PetApi,
        TagValues.STORE: StoreApi,
        TagValues.USER: UserApi,
    }
)
