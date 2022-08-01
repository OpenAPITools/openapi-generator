# do not import all endpoints into this module because that uses a lot of memory and stack frames
# if you need the ability to import all endpoints from this module, import them with
# from edu_sharing_client_api.apis.tag_to_api import tag_to_api

import enum


class TagValues(str, enum.Enum):
    ABOUT = "ABOUT"
    ADMIN_V1 = "ADMIN v1"
    ARCHIVE_V1 = "ARCHIVE v1"
    AUTHENTICATION_V1 = "AUTHENTICATION v1"
    BULK_V1 = "BULK v1"
    CLIENTUTILS_V1 = "CLIENTUTILS v1"
    COLLECTION_V1 = "COLLECTION v1"
    COMMENT_V1 = "COMMENT v1"
    CONFIG_V1 = "CONFIG v1"
    CONNECTOR_V1 = "CONNECTOR v1"
    IAM_V1 = "IAM v1"
    KNOWLEDGE_V1 = "KNOWLEDGE v1"
    LTI_V13 = "LTI v13"
    MDS_V1 = "MDS v1"
    MEDIACENTER_V1 = "MEDIACENTER v1"
    NETWORK_V1 = "NETWORK v1"
    NODE_V1 = "NODE v1"
    ORGANIZATION_V1 = "ORGANIZATION v1"
    RATING_V1 = "RATING v1"
    REGISTER_V1 = "REGISTER v1"
    RENDERING_V1 = "RENDERING v1"
    SEARCH_V1 = "SEARCH v1"
    SHARING_V1 = "SHARING v1"
    STATISTIC_V1 = "STATISTIC v1"
    STREAM_V1 = "STREAM v1"
    TOOL_V1 = "TOOL v1"
    TRACKING_V1 = "TRACKING v1"
    USAGE_V1 = "USAGE v1"
