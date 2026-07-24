print("✅ Script is running...")

from openapi_client import Configuration, ApiClient
from openapi_client.api.default_api import DefaultApi

configuration = Configuration()
with ApiClient(configuration) as api_client:
    api_instance = DefaultApi(api_client)
    print("✅ OpenAPI client instance created:", api_instance)
