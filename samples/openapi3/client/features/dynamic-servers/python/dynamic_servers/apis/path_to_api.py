import typing_extensions

from dynamic_servers.paths import PathValues
from dynamic_servers.apis.paths.default import Default
from dynamic_servers.apis.paths.custom import Custom

PathToApi = typing_extensions.TypedDict(
    'PathToApi',
    {
        PathValues.DEFAULT: Default,
        PathValues.CUSTOM: Custom,
    }
)

path_to_api = PathToApi(
    {
        PathValues.DEFAULT: Default,
        PathValues.CUSTOM: Custom,
    }
)
