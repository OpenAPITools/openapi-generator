import typing

from edu_sharing_client_api.apis.tags import TagValues
from edu_sharing_client_api.apis.tags.about_api import ABOUTApi
from edu_sharing_client_api.apis.tags.adminv1_api import ADMINV1Api
from edu_sharing_client_api.apis.tags.archivev1_api import ARCHIVEV1Api
from edu_sharing_client_api.apis.tags.authenticationv1_api import AUTHENTICATIONV1Api
from edu_sharing_client_api.apis.tags.bulkv1_api import BULKV1Api
from edu_sharing_client_api.apis.tags.clientutilsv1_api import CLIENTUTILSV1Api
from edu_sharing_client_api.apis.tags.collectionv1_api import COLLECTIONV1Api
from edu_sharing_client_api.apis.tags.commentv1_api import COMMENTV1Api
from edu_sharing_client_api.apis.tags.configv1_api import CONFIGV1Api
from edu_sharing_client_api.apis.tags.connectorv1_api import CONNECTORV1Api
from edu_sharing_client_api.apis.tags.iamv1_api import IAMV1Api
from edu_sharing_client_api.apis.tags.knowledgev1_api import KNOWLEDGEV1Api
from edu_sharing_client_api.apis.tags.ltiv13_api import LTIV13Api
from edu_sharing_client_api.apis.tags.mdsv1_api import MDSV1Api
from edu_sharing_client_api.apis.tags.mediacenterv1_api import MEDIACENTERV1Api
from edu_sharing_client_api.apis.tags.networkv1_api import NETWORKV1Api
from edu_sharing_client_api.apis.tags.nodev1_api import NODEV1Api
from edu_sharing_client_api.apis.tags.organizationv1_api import ORGANIZATIONV1Api
from edu_sharing_client_api.apis.tags.ratingv1_api import RATINGV1Api
from edu_sharing_client_api.apis.tags.registerv1_api import REGISTERV1Api
from edu_sharing_client_api.apis.tags.renderingv1_api import RENDERINGV1Api
from edu_sharing_client_api.apis.tags.searchv1_api import SEARCHV1Api
from edu_sharing_client_api.apis.tags.sharingv1_api import SHARINGV1Api
from edu_sharing_client_api.apis.tags.statisticv1_api import STATISTICV1Api
from edu_sharing_client_api.apis.tags.streamv1_api import STREAMV1Api
from edu_sharing_client_api.apis.tags.toolv1_api import TOOLV1Api
from edu_sharing_client_api.apis.tags.trackingv1_api import TRACKINGV1Api
from edu_sharing_client_api.apis.tags.usagev1_api import USAGEV1Api

TagToApi = typing.TypedDict(
    'TagToApi',
    {
        TagValues.ABOUT: ABOUTApi,
        TagValues.ADMIN_V1: ADMINV1Api,
        TagValues.ARCHIVE_V1: ARCHIVEV1Api,
        TagValues.AUTHENTICATION_V1: AUTHENTICATIONV1Api,
        TagValues.BULK_V1: BULKV1Api,
        TagValues.CLIENTUTILS_V1: CLIENTUTILSV1Api,
        TagValues.COLLECTION_V1: COLLECTIONV1Api,
        TagValues.COMMENT_V1: COMMENTV1Api,
        TagValues.CONFIG_V1: CONFIGV1Api,
        TagValues.CONNECTOR_V1: CONNECTORV1Api,
        TagValues.IAM_V1: IAMV1Api,
        TagValues.KNOWLEDGE_V1: KNOWLEDGEV1Api,
        TagValues.LTI_V13: LTIV13Api,
        TagValues.MDS_V1: MDSV1Api,
        TagValues.MEDIACENTER_V1: MEDIACENTERV1Api,
        TagValues.NETWORK_V1: NETWORKV1Api,
        TagValues.NODE_V1: NODEV1Api,
        TagValues.ORGANIZATION_V1: ORGANIZATIONV1Api,
        TagValues.RATING_V1: RATINGV1Api,
        TagValues.REGISTER_V1: REGISTERV1Api,
        TagValues.RENDERING_V1: RENDERINGV1Api,
        TagValues.SEARCH_V1: SEARCHV1Api,
        TagValues.SHARING_V1: SHARINGV1Api,
        TagValues.STATISTIC_V1: STATISTICV1Api,
        TagValues.STREAM_V1: STREAMV1Api,
        TagValues.TOOL_V1: TOOLV1Api,
        TagValues.TRACKING_V1: TRACKINGV1Api,
        TagValues.USAGE_V1: USAGEV1Api,
    }
)

tag_to_api = TagToApi(
    {
        TagValues.ABOUT: ABOUTApi,
        TagValues.ADMIN_V1: ADMINV1Api,
        TagValues.ARCHIVE_V1: ARCHIVEV1Api,
        TagValues.AUTHENTICATION_V1: AUTHENTICATIONV1Api,
        TagValues.BULK_V1: BULKV1Api,
        TagValues.CLIENTUTILS_V1: CLIENTUTILSV1Api,
        TagValues.COLLECTION_V1: COLLECTIONV1Api,
        TagValues.COMMENT_V1: COMMENTV1Api,
        TagValues.CONFIG_V1: CONFIGV1Api,
        TagValues.CONNECTOR_V1: CONNECTORV1Api,
        TagValues.IAM_V1: IAMV1Api,
        TagValues.KNOWLEDGE_V1: KNOWLEDGEV1Api,
        TagValues.LTI_V13: LTIV13Api,
        TagValues.MDS_V1: MDSV1Api,
        TagValues.MEDIACENTER_V1: MEDIACENTERV1Api,
        TagValues.NETWORK_V1: NETWORKV1Api,
        TagValues.NODE_V1: NODEV1Api,
        TagValues.ORGANIZATION_V1: ORGANIZATIONV1Api,
        TagValues.RATING_V1: RATINGV1Api,
        TagValues.REGISTER_V1: REGISTERV1Api,
        TagValues.RENDERING_V1: RENDERINGV1Api,
        TagValues.SEARCH_V1: SEARCHV1Api,
        TagValues.SHARING_V1: SHARINGV1Api,
        TagValues.STATISTIC_V1: STATISTICV1Api,
        TagValues.STREAM_V1: STREAMV1Api,
        TagValues.TOOL_V1: TOOLV1Api,
        TagValues.TRACKING_V1: TRACKINGV1Api,
        TagValues.USAGE_V1: USAGEV1Api,
    }
)
