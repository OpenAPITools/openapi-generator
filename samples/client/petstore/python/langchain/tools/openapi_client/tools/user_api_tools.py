# -*- coding: utf-8 -*-
from typing import List
from langchain.tools import StructuredTool
from openapi_client.api_client import ApiClient
from openapi_client.api.user_api import UserApi

def get_user_api_tools(api_client: ApiClient) -> List[StructuredTool]:
    """A factory function to create and return tools for the UserApi API."""

    api_instance = UserApi(api_client)
    tools = []

    create_user_tool = StructuredTool.from_function(
        func=api_instance.create_user,
        name="create_user",
        description="Create user"
    )
    tools.append(create_user_tool)

    create_users_with_array_input_tool = StructuredTool.from_function(
        func=api_instance.create_users_with_array_input,
        name="create_users_with_array_input",
        description="Creates list of users with given input array"
    )
    tools.append(create_users_with_array_input_tool)

    create_users_with_list_input_tool = StructuredTool.from_function(
        func=api_instance.create_users_with_list_input,
        name="create_users_with_list_input",
        description="Creates list of users with given input array"
    )
    tools.append(create_users_with_list_input_tool)

    delete_user_tool = StructuredTool.from_function(
        func=api_instance.delete_user,
        name="delete_user",
        description="Delete user"
    )
    tools.append(delete_user_tool)

    get_user_by_name_tool = StructuredTool.from_function(
        func=api_instance.get_user_by_name,
        name="get_user_by_name",
        description="Get user by user name"
    )
    tools.append(get_user_by_name_tool)

    login_user_tool = StructuredTool.from_function(
        func=api_instance.login_user,
        name="login_user",
        description="Logs user into the system"
    )
    tools.append(login_user_tool)

    logout_user_tool = StructuredTool.from_function(
        func=api_instance.logout_user,
        name="logout_user",
        description="Logs out current logged in user session"
    )
    tools.append(logout_user_tool)

    update_user_tool = StructuredTool.from_function(
        func=api_instance.update_user,
        name="update_user",
        description="Updated user"
    )
    tools.append(update_user_tool)

    return tools
