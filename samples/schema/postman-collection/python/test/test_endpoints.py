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

    def test_request_from_inline_examples(self):
        # item
        item = self.json_data['item'][0]['item'][0]['item'][0]
        self.assertEqual(item['name'], 'Example patch user')
        self.assertEqual(item['request']["method"], 'PATCH')
        self.assertEqual(item['request']["body"]["raw"], '{\n  "firstName" : "John",\n  "tags" : [ "user" ]\n}')

    def test_request_with_array_strings(self):
        # item
        item = self.json_data['item'][2]['item'][0]['item'][0]
        self.assertEqual(item['request']["method"], 'POST')
        data = json.loads(item['request']["body"]["raw"])
        # check is list
        self.assertTrue(isinstance(data.get("tags"), list))
        # check values
        self.assertTrue(set(data.get("tags")) == {"user", "admin", "guest"})

    def test_request_boolean_field(self):
        # item
        item = self.json_data['item'][0]['item'][0]['item'][1]
        self.assertEqual(item['name'], 'Example patch another user')
        self.assertEqual(item['request']["method"], 'PATCH')
        self.assertEqual(item['request']["body"]["raw"], '{\n  "firstName" : "Bill",\n  "tags" : [ "admin" ]\n}')


if __name__ == '__main__':
    unittest.main()
