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

    def _iter_items(self, items):
        for item in items:
            yield item
            children = item.get('item', [])
            if isinstance(children, list):
                for child in self._iter_items(children):
                    yield child

    def _find_item_by_name(self, name):
        for item in self._iter_items(self.json_data.get('item', [])):
            if item.get('name') == name:
                return item
        return None

    def test_endpoint_deprecated(self):
        path = self._find_item_by_name('/users/:userId (DEPRECATED)')
        self.assertIsNotNone(path)
        self.assertEqual(path['name'], '/users/:userId (DEPRECATED)')

    def test_request_from_inline_examples(self):
        item = self._find_item_by_name('Example patch user')
        self.assertIsNotNone(item)
        self.assertEqual(item['name'], 'Example patch user')
        self.assertEqual(item['request']["method"], 'PATCH')
        self.assertEqual(item['request']["body"]["raw"], '{\n  "firstName" : "John",\n  "tags" : [ "user" ]\n}')

    def test_request_with_array_strings(self):
        item = self._find_item_by_name('Example request for Get User')
        self.assertIsNotNone(item)
        self.assertEqual(item['request']["method"], 'POST')
        data = json.loads(item['request']["body"]["raw"])
        # check is list
        self.assertTrue(isinstance(data.get("tags"), list))
        # check values
        self.assertTrue(set(data.get("tags")) == {"user", "admin", "guest"})

    def test_request_boolean_field(self):
        item = self._find_item_by_name('Example patch another user')
        self.assertIsNotNone(item)
        self.assertEqual(item['name'], 'Example patch another user')
        self.assertEqual(item['request']["method"], 'PATCH')
        self.assertEqual(item['request']["body"]["raw"], '{\n  "firstName" : "Bill",\n  "tags" : [ "admin" ]\n}')


if __name__ == '__main__':
    unittest.main()
