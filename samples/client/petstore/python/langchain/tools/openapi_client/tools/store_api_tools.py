# -*- coding: utf-8 -*-
from typing import List
from langchain.tools import StructuredTool
from openapi_client.api_client import ApiClient
from openapi_client.api.store_api import StoreApi

def get_store_api_tools(api_client: ApiClient) -> List[StructuredTool]:
    """A factory function to create and return tools for the StoreApi API."""

    api_instance = StoreApi(api_client)
    tools = []

    delete_order_tool = StructuredTool.from_function(
        func=api_instance.delete_order,
        name="delete_order",
        description="Delete purchase order by ID"
    )
    tools.append(delete_order_tool)

    get_inventory_tool = StructuredTool.from_function(
        func=api_instance.get_inventory,
        name="get_inventory",
        description="Returns pet inventories by status"
    )
    tools.append(get_inventory_tool)

    get_order_by_id_tool = StructuredTool.from_function(
        func=api_instance.get_order_by_id,
        name="get_order_by_id",
        description="Find purchase order by ID"
    )
    tools.append(get_order_by_id_tool)

    place_order_tool = StructuredTool.from_function(
        func=api_instance.place_order,
        name="place_order",
        description="Place an order for a pet"
    )
    tools.append(place_order_tool)

    return tools
