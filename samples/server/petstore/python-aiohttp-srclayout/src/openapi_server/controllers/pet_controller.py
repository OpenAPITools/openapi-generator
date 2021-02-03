from typing import List, Dict
from aiohttp import web

from openapi_server.models.api_response import ApiResponse
from openapi_server.models.pet import Pet
from openapi_server import util


async def add_pet(request: web.Request, body) -> web.Response:
    """Add a new pet to the store

    

    :param body: Pet object that needs to be added to the store
    :type body: dict | bytes

    """
    body = Pet.from_dict(body)
    return web.Response(status=200)


async def delete_pet(request: web.Request, pet_id, api_key=None) -> web.Response:
    """Deletes a pet

    

    :param pet_id: Pet id to delete
    :type pet_id: int
    :param api_key: 
    :type api_key: str

    """
    return web.Response(status=200)


async def find_pets_by_status(request: web.Request, status) -> web.Response:
    """Finds Pets by status

    Multiple status values can be provided with comma separated strings

    :param status: Status values that need to be considered for filter
    :type status: List[str]

    """
    return web.Response(status=200)


async def find_pets_by_tags(request: web.Request, tags) -> web.Response:
    """Finds Pets by tags

    Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.

    :param tags: Tags to filter by
    :type tags: List[str]

    """
    return web.Response(status=200)


async def get_pet_by_id(request: web.Request, pet_id) -> web.Response:
    """Find pet by ID

    Returns a single pet

    :param pet_id: ID of pet to return
    :type pet_id: int

    """
    return web.Response(status=200)


async def update_pet(request: web.Request, body) -> web.Response:
    """Update an existing pet

    

    :param body: Pet object that needs to be added to the store
    :type body: dict | bytes

    """
    body = Pet.from_dict(body)
    return web.Response(status=200)


async def update_pet_with_form(request: web.Request, pet_id, name=None, status=None) -> web.Response:
    """Updates a pet in the store with form data

    

    :param pet_id: ID of pet that needs to be updated
    :type pet_id: int
    :param name: Updated name of the pet
    :type name: str
    :param status: Updated status of the pet
    :type status: str

    """
    return web.Response(status=200)


async def upload_file(request: web.Request, pet_id, additional_metadata=None, file=None) -> web.Response:
    """uploads an image

    

    :param pet_id: ID of pet to update
    :type pet_id: int
    :param additional_metadata: Additional data to pass to server
    :type additional_metadata: str
    :param file: file to upload
    :type file: str

    """
    return web.Response(status=200)
