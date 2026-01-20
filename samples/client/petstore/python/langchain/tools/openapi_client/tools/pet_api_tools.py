# -*- coding: utf-8 -*-
from typing import List
from langchain.tools import StructuredTool
from openapi_client.api_client import ApiClient
from openapi_client.api.pet_api import PetApi

def get_pet_api_tools(api_client: ApiClient) -> List[StructuredTool]:
    """A factory function to create and return tools for the PetApi API."""

    api_instance = PetApi(api_client)
    tools = []

    add_pet_tool = StructuredTool.from_function(
        func=api_instance.add_pet,
        name="add_pet",
        description="Add a new pet to the store"
    )
    tools.append(add_pet_tool)

    delete_pet_tool = StructuredTool.from_function(
        func=api_instance.delete_pet,
        name="delete_pet",
        description="Deletes a pet"
    )
    tools.append(delete_pet_tool)

    find_pets_by_status_tool = StructuredTool.from_function(
        func=api_instance.find_pets_by_status,
        name="find_pets_by_status",
        description="Finds Pets by status"
    )
    tools.append(find_pets_by_status_tool)

    find_pets_by_tags_tool = StructuredTool.from_function(
        func=api_instance.find_pets_by_tags,
        name="find_pets_by_tags",
        description="Finds Pets by tags"
    )
    tools.append(find_pets_by_tags_tool)

    get_pet_by_id_tool = StructuredTool.from_function(
        func=api_instance.get_pet_by_id,
        name="get_pet_by_id",
        description="Find pet by ID"
    )
    tools.append(get_pet_by_id_tool)

    update_pet_tool = StructuredTool.from_function(
        func=api_instance.update_pet,
        name="update_pet",
        description="Update an existing pet"
    )
    tools.append(update_pet_tool)

    update_pet_with_form_tool = StructuredTool.from_function(
        func=api_instance.update_pet_with_form,
        name="update_pet_with_form",
        description="Updates a pet in the store with form data"
    )
    tools.append(update_pet_with_form_tool)

    upload_file_tool = StructuredTool.from_function(
        func=api_instance.upload_file,
        name="upload_file",
        description="uploads an image"
    )
    tools.append(upload_file_tool)

    return tools
