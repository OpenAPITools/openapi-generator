# -*- coding: utf-8 -*-
"""
An aggregator for all generated LangChain tools.

This file provides a single list, `all_tools`, that you can import and
provide to your LangChain agent.
"""
from typing import List
from langchain.tools import StructuredTool

from openapi_client.api_client import ApiClient
from openapi_client.configuration import Configuration

# Import the factory functions from each tool module
from openapi_client.tools.pet_api_tools import get_pet_api_tools
from openapi_client.tools.store_api_tools import get_store_api_tools
from openapi_client.tools.user_api_tools import get_user_api_tools

def get_all_tools() -> List[StructuredTool]:
    """
    Initializes the API client and aggregates tools from all API modules.
    """
    # TODO: You may need to customize this based on your API's authentication needs.
    configuration = Configuration(host="")

    all_tools_list = []

    with ApiClient(configuration) as api_client:
            all_tools_list.extend(get_pet_api_tools(api_client))
            all_tools_list.extend(get_store_api_tools(api_client))
            all_tools_list.extend(get_user_api_tools(api_client))

    return all_tools_list

# A pre-initialized list of all tools for convenience.
all_tools = get_all_tools()
