# coding: utf-8

from __future__ import absolute_import

from flask import json
from six import BytesIO

from openapi_server.models.order import Order  # noqa: E501
from openapi_server.test import BaseTestCase


class TestStoreController(BaseTestCase):
    """StoreController integration test stubs"""

    def test_delete_order(self):
        """Test case for delete_order

        Delete purchase order by ID
        """
        response = self.client.open(
            '/v2/store/order/{orderId}'.format(order_id='order_id_example'),
            method='DELETE')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_get_inventory(self):
        """Test case for get_inventory

        Returns pet inventories by status
        """
        response = self.client.open(
            '/v2/store/inventory',
            method='GET')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_get_order_by_id(self):
        """Test case for get_order_by_id

        Find purchase order by ID
        """
        response = self.client.open(
            '/v2/store/order/{orderId}'.format(order_id=5),
            method='GET')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_place_order(self):
        """Test case for place_order

        Place an order for a pet
        """
        order = Order()
        response = self.client.open(
            '/v2/store/order',
            method='POST',
            data=json.dumps(order),
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))


if __name__ == '__main__':
    import unittest
    unittest.main()
