# coding: utf-8

from typing import Dict, List  # noqa: F401

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
from openapi_server.models.order import Order
from openapi_server.security_api import get_token_api_key

router = APIRouter()


@router.delete(
    "/store/order/{orderId}",
    responses={
        400: {"description": "Invalid ID supplied"},
        404: {"description": "Order not found"},
    },
    tags=["store"],
    summary="Delete purchase order by ID",
)
async def delete_order(
    orderId: str = Path(None, description="ID of the order that needs to be deleted"),
) -> None:
    """For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors"""
    ...


@router.get(
    "/store/inventory",
    responses={
        200: {"model": Dict[str, int], "description": "successful operation"},
    },
    tags=["store"],
    summary="Returns pet inventories by status",
)
async def get_inventory(
    token_api_key: TokenModel = Security(
        get_token_api_key
    ),
) -> Dict[str, int]:
    """Returns a map of status codes to quantities"""
    ...


@router.get(
    "/store/order/{orderId}",
    responses={
        200: {"model": Order, "description": "successful operation"},
        400: {"description": "Invalid ID supplied"},
        404: {"description": "Order not found"},
    },
    tags=["store"],
    summary="Find purchase order by ID",
)
async def get_order_by_id(
    orderId: int = Path(None, description="ID of pet that needs to be fetched", ge=1, le=5),
) -> Order:
    """For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions"""
    ...


@router.post(
    "/store/order",
    responses={
        200: {"model": Order, "description": "successful operation"},
        400: {"description": "Invalid Order"},
    },
    tags=["store"],
    summary="Place an order for a pet",
)
async def place_order(
    order: Order = Body(None, description="order placed for purchasing the pet"),
) -> Order:
    ...
