# coding: utf-8

# flake8: noqa

import os
import time
import unittest

import petstore_api


class OrderModelTests(unittest.TestCase):

    def test_status(self):
        order = petstore_api.Order()
        # order.status = "placed"
        # self.assertEqual("placed", order.status)

        # with self.assertRaises(ValueError):
        #    order.status = "invalid"
