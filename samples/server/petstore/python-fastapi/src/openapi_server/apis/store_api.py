# coding: utf-8

from typing import Dict, List  # noqa: F401
import importlib
import pkgutil

from openapi_server.apis.store_api_base import BaseStoreApi
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
from pydantic import Field, StrictInt, StrictStr
from typing import Any, Dict
from typing_extensions import Annotated
from openapi_server.models.order import Order
from openapi_server.security_api import get_token_api_key

router = APIRouter()

ns_pkg = openapi_server.impl
for _, name, _ in pkgutil.iter_modules(ns_pkg.__path__, ns_pkg.__name__ + "."):
    importlib.import_module(name)


@router.delete(
    "/store/order/{orderId}",
    responses={
        400: {"description": "Invalid ID supplied"},
        404: {"description": "Order not found"},
    },
    tags=["store"],
    summary="Delete purchase order by ID",
    response_model_by_alias=True,
)
async def delete_order(
    orderId: Annotated[StrictStr, Field(description="ID of the order that needs to be deleted")] = Path(..., description="ID of the order that needs to be deleted"),
) -> None:
    """For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors"""
    if not BaseStoreApi.subclasses:
        raise HTTPException(status_code=500, detail="Not implemented")
    return await BaseStoreApi.subclasses[0]().delete_order(orderId)


@router.get(
    "/store/inventory",
    responses={
        200: {"model": Dict[str, int], "description": "successful operation"},
    },
    tags=["store"],
    summary="Returns pet inventories by status",
    response_model_by_alias=True,
)
async def get_inventory(
    token_api_key: TokenModel = Security(
        get_token_api_key
    ),
) -> Dict[str, int]:
    """Returns a map of status codes to quantities"""
    if not BaseStoreApi.subclasses:
        raise HTTPException(status_code=500, detail="Not implemented")
    return await BaseStoreApi.subclasses[0]().get_inventory()


@router.get(
    "/store/order/{orderId}",
    responses={
        200: {"model": Order, "description": "successful operation"},
        400: {"description": "Invalid ID supplied"},
        404: {"description": "Order not found"},
    },
    tags=["store"],
    summary="Find purchase order by ID",
    response_model_by_alias=True,
)
async def get_order_by_id(
    orderId: Annotated[int, Field(le=5, strict=True, ge=1, description="ID of pet that needs to be fetched")] = Path(..., description="ID of pet that needs to be fetched", ge=1, le=5),
) -> Order:
    """For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generate exceptions"""
    if not BaseStoreApi.subclasses:
        raise HTTPException(status_code=500, detail="Not implemented")
    return await BaseStoreApi.subclasses[0]().get_order_by_id(orderId)


@router.post(
    "/store/order",
    responses={
        200: {"model": Order, "description": "successful operation"},
        400: {"description": "Invalid Order"},
    },
    tags=["store"],
    summary="Place an order for a pet",
    response_model_by_alias=True,
)
async def place_order(
    order: Annotated[Order, Field(description="order placed for purchasing the pet")] = Body(None, description="order placed for purchasing the pet"),
) -> Order:
    """"""
    if not BaseStoreApi.subclasses:
        raise HTTPException(status_code=500, detail="Not implemented")
    return await BaseStoreApi.subclasses[0]().place_order(order)
