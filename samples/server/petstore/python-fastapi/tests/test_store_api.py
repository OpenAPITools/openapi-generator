# coding: utf-8

from fastapi.testclient import TestClient


from openapi_server.models.order import Order  # noqa: F401


def test_delete_order(client: TestClient):
    """Test case for delete_order

    Delete purchase order by ID
    """

    headers = {
    }
    response = client.request(
        "DELETE",
        "/store/order/{orderId}".format(orderId='order_id_example'),
        headers=headers,
    )

    # uncomment below to assert the status code of the HTTP response
    #assert response.status_code == 200


def test_get_inventory(client: TestClient):
    """Test case for get_inventory

    Returns pet inventories by status
    """

    headers = {
        "api_key": "special-key",
    }
    response = client.request(
        "GET",
        "/store/inventory",
        headers=headers,
    )

    # uncomment below to assert the status code of the HTTP response
    #assert response.status_code == 200


def test_get_order_by_id(client: TestClient):
    """Test case for get_order_by_id

    Find purchase order by ID
    """

    headers = {
    }
    response = client.request(
        "GET",
        "/store/order/{orderId}".format(orderId=56),
        headers=headers,
    )

    # uncomment below to assert the status code of the HTTP response
    #assert response.status_code == 200


def test_place_order(client: TestClient):
    """Test case for place_order

    Place an order for a pet
    """
    order = {"pet_id":6,"quantity":1,"id":0,"ship_date":"2000-01-23T04:56:07.000+00:00","complete":0,"status":"placed"}

    headers = {
    }
    response = client.request(
        "POST",
        "/store/order",
        headers=headers,
        json=order,
    )

    # uncomment below to assert the status code of the HTTP response
    #assert response.status_code == 200

