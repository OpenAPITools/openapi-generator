#!/usr/bin/env python

import sys
import unittest
import datetime
import random

from BaseApiTest import BaseApiTest

sys.path = ['./'] + sys.path
from petstore import *
from petstore.models import *


class StoreApiTest(BaseApiTest):

    @classmethod
    def setUpClass(cls):
        # super(PetApiTest, self).setUp()
        cls.randomId = int(9500 * random.random()) + 500

    def testGetOrderById(self):
        res = self.storeApi.getOrderById(1)
        assert res, 'null getOrderById result'
        assert 1 == res.id, 'order id should be int(1)'

    def testDeleteOrder(self):
        self.storeApi.deleteOrder(3)
        self.storeApi.deleteOrder("foo")

    def testPlaceOrder(self):

        order = Order.Order()
        order.id = self.randomId
        order.petId = 1
        order.status = 'ordered'
        order.quantity = 10
        order.shipDate = datetime.datetime.strptime("2011-01-09T13:55:07.123",
                                                    "%Y-%m-%dT%H:%M:%S.%f")
        self.storeApi.placeOrder(order)

        new_order = self.storeApi.getOrderById(self.randomId)
        assert new_order.id == new_order.id, 'ids should match'
        assert new_order.petId == new_order.petId, 'petIds should match'
        assert new_order.status == new_order.status, 'status should match'
        assert new_order.quantity == new_order.quantity, 'quantity should match'
        assert new_order.shipDate == new_order.shipDate, 'shipDate should match'

if __name__ == "__main__":
    unittest.main()
