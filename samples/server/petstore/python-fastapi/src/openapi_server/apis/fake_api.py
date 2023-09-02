# coding: utf-8

from typing import Dict, List  # noqa: F401
import importlib
import pkgutil

from openapi_server.apis.fake_api_base import BaseFakeApi
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


router = APIRouter()

ns_pkg = openapi_server.impl
for _, name, _ in pkgutil.iter_modules(ns_pkg.__path__, ns_pkg.__name__ + "."):
    importlib.import_module(name)


@router.get(
    "/fake/query_param_default",
    responses={
        400: {"description": "Invalid username supplied"},
        404: {"description": "User not found"},
    },
    tags=["fake"],
    summary="test query parameter default value",
    response_model_by_alias=True,
)
async def fake_query_param_default(
    has_default: str = Query('Hello World', description="has default value"),
    no_default: str = Query(None, description="no default value"),
) -> None:
    """"""
    return BaseFakeApi.subclasses[0]().fake_query_param_default(has_default, no_default)
