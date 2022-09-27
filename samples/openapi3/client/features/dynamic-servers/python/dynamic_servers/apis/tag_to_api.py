import typing_extensions

from dynamic_servers.apis.tags import TagValues
from dynamic_servers.apis.tags.usage_api import UsageApi

TagToApi = typing_extensions.TypedDict(
    'TagToApi',
    {
        TagValues.USAGE: UsageApi,
    }
)

tag_to_api = TagToApi(
    {
        TagValues.USAGE: UsageApi,
    }
)
