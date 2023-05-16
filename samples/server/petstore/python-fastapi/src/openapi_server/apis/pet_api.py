# coding: utf-8

from typing import Dict, List  # noqa: F401
import importlib
import pkgutil

from openapi_server.apis.pet_api_base import BasePetApi
import openapi_server.impl

from fastapi import (  # noqa: F401
    APIRouter,
    Body,
    Cookie,
    Depends,
    Form,
    Header,
    Path,
    Query,
    Response,
    Security,
    status,
)

from openapi_server.models.extra_models import TokenModel  # noqa: F401
from openapi_server.models.api_response import ApiResponse
from openapi_server.models.pet import Pet
from openapi_server.security_api import get_token_petstore_auth, get_token_api_key

router = APIRouter()

ns_pkg = openapi_server.impl
for _, name, _ in pkgutil.iter_modules(ns_pkg.__path__, ns_pkg.__name__ + "."):
    importlib.import_module(name)


@router.post(
    "/pet",
    responses={
        200: {"model": Pet, "description": "successful operation"},
        405: {"description": "Invalid input"},
    },
    tags=["pet"],
    summary="Add a new pet to the store",
    response_model_by_alias=True,
)
async def add_pet(
    pet: Pet = Body(None, description="Pet object that needs to be added to the store"),
    token_petstore_auth: TokenModel = Security(
        get_token_petstore_auth, scopes=["write:pets", "read:pets"]
    ),
) -> Pet:
    """"""
    return BasePetApi.subclasses[0]().add_pet(pet)


@router.delete(
    "/pet/{petId}",
    responses={
        400: {"description": "Invalid pet value"},
    },
    tags=["pet"],
    summary="Deletes a pet",
    response_model_by_alias=True,
)
async def delete_pet(
    petId: int = Path(None, description="Pet id to delete"),
    api_key: str = Header(None, description=""),
    token_petstore_auth: TokenModel = Security(
        get_token_petstore_auth, scopes=["write:pets", "read:pets"]
    ),
) -> None:
    """"""
    return BasePetApi.subclasses[0]().delete_pet(petId, api_key)


@router.get(
    "/pet/findByStatus",
    responses={
        200: {"model": List[Pet], "description": "successful operation"},
        400: {"description": "Invalid status value"},
    },
    tags=["pet"],
    summary="Finds Pets by status",
    response_model_by_alias=True,
)
async def find_pets_by_status(
    status: List[str] = Query(None, description="Status values that need to be considered for filter"),
    token_petstore_auth: TokenModel = Security(
        get_token_petstore_auth, scopes=["read:pets"]
    ),
) -> List[Pet]:
    """Multiple status values can be provided with comma separated strings"""
    return BasePetApi.subclasses[0]().find_pets_by_status(status)


@router.get(
    "/pet/findByTags",
    responses={
        200: {"model": List[Pet], "description": "successful operation"},
        400: {"description": "Invalid tag value"},
    },
    tags=["pet"],
    summary="Finds Pets by tags",
    response_model_by_alias=True,
)
async def find_pets_by_tags(
    tags: List[str] = Query(None, description="Tags to filter by"),
    token_petstore_auth: TokenModel = Security(
        get_token_petstore_auth, scopes=["read:pets"]
    ),
) -> List[Pet]:
    """Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing."""
    return BasePetApi.subclasses[0]().find_pets_by_tags(tags)


@router.get(
    "/pet/{petId}",
    responses={
        200: {"model": Pet, "description": "successful operation"},
        400: {"description": "Invalid ID supplied"},
        404: {"description": "Pet not found"},
    },
    tags=["pet"],
    summary="Find pet by ID",
    response_model_by_alias=True,
)
async def get_pet_by_id(
    petId: int = Path(None, description="ID of pet to return"),
    token_api_key: TokenModel = Security(
        get_token_api_key
    ),
) -> Pet:
    """Returns a single pet"""
    return BasePetApi.subclasses[0]().get_pet_by_id(petId)


@router.put(
    "/pet",
    responses={
        200: {"model": Pet, "description": "successful operation"},
        400: {"description": "Invalid ID supplied"},
        404: {"description": "Pet not found"},
        405: {"description": "Validation exception"},
    },
    tags=["pet"],
    summary="Update an existing pet",
    response_model_by_alias=True,
)
async def update_pet(
    pet: Pet = Body(None, description="Pet object that needs to be added to the store"),
    token_petstore_auth: TokenModel = Security(
        get_token_petstore_auth, scopes=["write:pets", "read:pets"]
    ),
) -> Pet:
    """"""
    return BasePetApi.subclasses[0]().update_pet(pet)


@router.post(
    "/pet/{petId}",
    responses={
        405: {"description": "Invalid input"},
    },
    tags=["pet"],
    summary="Updates a pet in the store with form data",
    response_model_by_alias=True,
)
async def update_pet_with_form(
    petId: int = Path(None, description="ID of pet that needs to be updated"),
    name: str = Form(None, description="Updated name of the pet"),
    status: str = Form(None, description="Updated status of the pet"),
    token_petstore_auth: TokenModel = Security(
        get_token_petstore_auth, scopes=["write:pets", "read:pets"]
    ),
) -> None:
    """"""
    return BasePetApi.subclasses[0]().update_pet_with_form(petId, name, status)


@router.post(
    "/pet/{petId}/uploadImage",
    responses={
        200: {"model": ApiResponse, "description": "successful operation"},
    },
    tags=["pet"],
    summary="uploads an image",
    response_model_by_alias=True,
)
async def upload_file(
    petId: int = Path(None, description="ID of pet to update"),
    additional_metadata: str = Form(None, description="Additional data to pass to server"),
    file: str = Form(None, description="file to upload"),
    token_petstore_auth: TokenModel = Security(
        get_token_petstore_auth, scopes=["write:pets", "read:pets"]
    ),
) -> ApiResponse:
    """"""
    return BasePetApi.subclasses[0]().upload_file(petId, additional_metadata, file)
