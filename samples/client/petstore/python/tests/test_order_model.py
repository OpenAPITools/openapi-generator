# coding: utf-8

"""
Run the tests.
$ pip install nose (optional)
$ cd swagger_client-python
$ nosetests -v
"""

import os
import time
import unittest

import swagger_client


class OrderModelTests(unittest.TestCase):

    def test_status(self):
        order = swagger_client.Order()
        order.status = "placed"
        self.assertEqual("placed", order.status)

        with self.assertRaises(ValueError):
            order.status = "invalid"
