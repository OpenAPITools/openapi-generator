"""
    Testing Postman generator output
"""
from __future__ import absolute_import

import unittest

import json


class TestParameters(unittest.TestCase):

    def setUp(self):
        with open('./postman.json', 'r') as file:
            self.json_data = json.load(file)

    def tearDown(self):
        pass

    def test_endpoint_deprecated(self):
        # path
        path = self.json_data['item'][0]['item'][0]
        self.assertEqual(path['name'], '/users/:userId (DEPRECATED)')


if __name__ == '__main__':
    unittest.main()
