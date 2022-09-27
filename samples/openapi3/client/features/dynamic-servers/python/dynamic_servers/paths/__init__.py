# do not import all endpoints into this module because that uses a lot of memory and stack frames
# if you need the ability to import all endpoints from this module, import them with
# from dynamic_servers.apis.path_to_api import path_to_api

import enum


class PathValues(str, enum.Enum):
    DEFAULT = "/default"
    CUSTOM = "/custom"
