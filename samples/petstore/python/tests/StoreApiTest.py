#!/usr/bin/env python

import sys
import unittest
import datetime

from BaseApiTest import BaseApiTest

sys.path = ['./'] + sys.path
from petstore import *
from petstore.models import *


class StoreApiTest(BaseApiTest):

    def testGetOrderById(self):
        res = self.storeApi.getOrderById(1)
        assert res, 'null getOrderById result'
        assert long(1) == res.id, 'order id should be long(1)'

    def testDeleteOrder(self):
        self.storeApi.deleteOrder(3)
        self.storeApi.deleteOrder("foo")

    def testPlaceOrder(self):
        order = Order.Order()
        order.petId = 1
        order.status = 'ordered'
        order.quantity = 10
        order.shipDate = datetime.datetime.strptime("2011-01-09T13:55:07.123",
                                                    "%Y-%m-%dT%H:%M:%S.%f")
        self.storeApi.placeOrder(order)


if __name__ == "__main__":
    unittest.main()
