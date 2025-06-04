# coding: utf-8

from typing import Dict, List  # noqa: F401
import importlib
import pkgutil

from openapi_server.apis.user_api_base import BaseUserApi
import openapi_server.impl

from fastapi import (  # noqa: F401
    APIRouter,
    Body,
    Cookie,
    Depends,
    Form,
    Header,
    HTTPException,
    Path,
    Query,
    Response,
    Security,
    status,
)

from openapi_server.models.extra_models import TokenModel  # noqa: F401
from pydantic import Field, StrictStr, field_validator
from typing import Any, List
from typing_extensions import Annotated
from openapi_server.models.user import User
from openapi_server.security_api import get_token_api_key

router = APIRouter()

ns_pkg = openapi_server.impl
for _, name, _ in pkgutil.iter_modules(ns_pkg.__path__, ns_pkg.__name__ + "."):
    importlib.import_module(name)


@router.post(
    "/user",
    responses={
        "default": {"description": "successful operation"},
    },
    tags=["user"],
    summary="Create user",
    response_model_by_alias=True,
)
async def create_user(
    user: Annotated[User, Field(description="Created user object")] = Body(None, description="Created user object"),
    token_api_key: TokenModel = Security(
        get_token_api_key
    ),
) -> None:
    """This can only be done by the logged in user."""
    if not BaseUserApi.subclasses:
        raise HTTPException(status_code=500, detail="Not implemented")
    return await BaseUserApi.subclasses[0]().create_user(user)


@router.post(
    "/user/createWithArray",
    responses={
        "default": {"description": "successful operation"},
    },
    tags=["user"],
    summary="Creates list of users with given input array",
    response_model_by_alias=True,
)
async def create_users_with_array_input(
    user: Annotated[List[User], Field(description="List of user object")] = Body(None, description="List of user object"),
    token_api_key: TokenModel = Security(
        get_token_api_key
    ),
) -> None:
    """"""
    if not BaseUserApi.subclasses:
        raise HTTPException(status_code=500, detail="Not implemented")
    return await BaseUserApi.subclasses[0]().create_users_with_array_input(user)


@router.post(
    "/user/createWithList",
    responses={
        "default": {"description": "successful operation"},
    },
    tags=["user"],
    summary="Creates list of users with given input array",
    response_model_by_alias=True,
)
async def create_users_with_list_input(
    user: Annotated[List[User], Field(description="List of user object")] = Body(None, description="List of user object"),
    token_api_key: TokenModel = Security(
        get_token_api_key
    ),
) -> None:
    """"""
    if not BaseUserApi.subclasses:
        raise HTTPException(status_code=500, detail="Not implemented")
    return await BaseUserApi.subclasses[0]().create_users_with_list_input(user)


@router.delete(
    "/user/{username}",
    responses={
        400: {"description": "Invalid username supplied"},
        404: {"description": "User not found"},
    },
    tags=["user"],
    summary="Delete user",
    response_model_by_alias=True,
)
async def delete_user(
    username: Annotated[StrictStr, Field(description="The name that needs to be deleted")] = Path(..., description="The name that needs to be deleted"),
    token_api_key: TokenModel = Security(
        get_token_api_key
    ),
) -> None:
    """This can only be done by the logged in user."""
    if not BaseUserApi.subclasses:
        raise HTTPException(status_code=500, detail="Not implemented")
    return await BaseUserApi.subclasses[0]().delete_user(username)


@router.get(
    "/user/{username}",
    responses={
        200: {"model": User, "description": "successful operation"},
        400: {"description": "Invalid username supplied"},
        404: {"description": "User not found"},
    },
    tags=["user"],
    summary="Get user by user name",
    response_model_by_alias=True,
)
async def get_user_by_name(
    username: Annotated[StrictStr, Field(description="The name that needs to be fetched. Use user1 for testing.")] = Path(..., description="The name that needs to be fetched. Use user1 for testing."),
) -> User:
    """"""
    if not BaseUserApi.subclasses:
        raise HTTPException(status_code=500, detail="Not implemented")
    return await BaseUserApi.subclasses[0]().get_user_by_name(username)


@router.get(
    "/user/login",
    responses={
        200: {"model": str, "description": "successful operation"},
        400: {"description": "Invalid username/password supplied"},
    },
    tags=["user"],
    summary="Logs user into the system",
    response_model_by_alias=True,
)
async def login_user(
    username: Annotated[str, Field(strict=True, description="The user name for login")] = Query(None, description="The user name for login", alias="username", regex=r"/^[a-zA-Z0-9]+[a-zA-Z0-9\.\-_]*[a-zA-Z0-9]+$/"),
    password: Annotated[StrictStr, Field(description="The password for login in clear text")] = Query(None, description="The password for login in clear text", alias="password"),
) -> str:
    """"""
    if not BaseUserApi.subclasses:
        raise HTTPException(status_code=500, detail="Not implemented")
    return await BaseUserApi.subclasses[0]().login_user(username, password)


@router.get(
    "/user/logout",
    responses={
        "default": {"description": "successful operation"},
    },
    tags=["user"],
    summary="Logs out current logged in user session",
    response_model_by_alias=True,
)
async def logout_user(
    token_api_key: TokenModel = Security(
        get_token_api_key
    ),
) -> None:
    """"""
    if not BaseUserApi.subclasses:
        raise HTTPException(status_code=500, detail="Not implemented")
    return await BaseUserApi.subclasses[0]().logout_user()


@router.put(
    "/user/{username}",
    responses={
        400: {"description": "Invalid user supplied"},
        404: {"description": "User not found"},
    },
    tags=["user"],
    summary="Updated user",
    response_model_by_alias=True,
)
async def update_user(
    username: Annotated[StrictStr, Field(description="name that need to be deleted")] = Path(..., description="name that need to be deleted"),
    user: Annotated[User, Field(description="Updated user object")] = Body(None, description="Updated user object"),
    token_api_key: TokenModel = Security(
        get_token_api_key
    ),
) -> None:
    """This can only be done by the logged in user."""
    if not BaseUserApi.subclasses:
        raise HTTPException(status_code=500, detail="Not implemented")
    return await BaseUserApi.subclasses[0]().update_user(username, user)
