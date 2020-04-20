# coding: utf-8

import pytest
import json
from aiohttp import web

from openapi_server.models.order import Order


async def test_delete_order(client):
    """Test case for delete_order

    Delete purchase order by ID
    """
    headers = { 
    }
    response = await client.request(
        method='DELETE',
        path='/v2/store/order/{order_id}'.format(order_id='order_id_example'),
        headers=headers,
        )
    assert response.status == 200, 'Response body is : ' + (await response.read()).decode('utf-8')


async def test_get_inventory(client):
    """Test case for get_inventory

    Returns pet inventories by status
    """
    headers = { 
        'Accept': 'application/json',
        'api_key': 'special-key',
    }
    response = await client.request(
        method='GET',
        path='/v2/store/inventory',
        headers=headers,
        )
    assert response.status == 200, 'Response body is : ' + (await response.read()).decode('utf-8')


async def test_get_order_by_id(client):
    """Test case for get_order_by_id

    Find purchase order by ID
    """
    headers = { 
        'Accept': 'application/json',
    }
    response = await client.request(
        method='GET',
        path='/v2/store/order/{order_id}'.format(order_id=5),
        headers=headers,
        )
    assert response.status == 200, 'Response body is : ' + (await response.read()).decode('utf-8')


@pytest.mark.skip("*/* not supported by Connexion. Use application/json instead. See https://github.com/zalando/connexion/pull/760")
async def test_place_order(client):
    """Test case for place_order

    Place an order for a pet
    """
    body = {}
    headers = { 
        'Accept': 'application/json',
        'Content-Type': 'application/json',
    }
    response = await client.request(
        method='POST',
        path='/v2/store/order',
        headers=headers,
        json=body,
        )
    assert response.status == 200, 'Response body is : ' + (await response.read()).decode('utf-8')

