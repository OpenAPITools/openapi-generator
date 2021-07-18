# coding: utf-8

from __future__ import absolute_import
import unittest
from datetime import datetime

from openapi_server.models.api_response import ApiResponse  # noqa: E501
from openapi_server.models.order import Order
from openapi_server.test import BaseTestCase


class TestOrderAttributeMap(BaseTestCase):
    """Model serialization has to respect attribute_map on the class"""

    def setUp(self) -> None:
        now = datetime.now()

        self.data = {
            "id": 0,
            "pet_id": 1,
            "quantity": 25,
            "ship_date": now,
            "status": "placed",
            "complete": True
        }

        self.desired_data = {
            "id": 0,
            "petId": 1,
            "quantity": 25,
            "shipDate": now,
            "status": "placed",
            "complete": True
        }

    def test_order_to_dict_default(self):
        """
        By default `attribute_map` uses `True` value,
        attribute_map of the class is respected.
        """
        order = Order(**self.data)
        dikt = order.to_dict()
        self.assertEqual(dikt, self.desired_data)

    def test_order_to_dict_false(self):
        """
        Passing `False` into `to_dict` method does not use
        `attribute_map` on the class.
        """
        order = Order(**self.data)
        dikt = order.to_dict(attr_map=False)
        self.assertEqual(dikt, self.data)

    def test_order_from_dict_to_dict(self):
        """
        Creates new `Order` instance by using `from_dict` method,
        passing in data from existing `Order` `to_dict` method.
        Instances and their `dict` data are equal.
        """
        order = Order(**self.data)
        order2 = Order.from_dict(order.to_dict())

        self.assertEqual(order.to_dict(), order2.to_dict())
        self.assertEqual(order, order2)


if __name__ == '__main__':
    unittest.main()
