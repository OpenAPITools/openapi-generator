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

    def _get_query_param_request(self):
        item = self._find_item_by_name('Get User Info by Query Param')
        self.assertIsNotNone(item)
        return item['request']

    def test_request_parameter_description(self):
        request = self._get_query_param_request()
        self.assertEqual(request['url']['raw'], '{{baseUrl}}/users/')
        # first query parameter
        self.assertEqual(request['url']['query'][0]['key'], 'pUserId')
        self.assertEqual(request['url']['query'][0]['value'], '888')
        self.assertEqual(request['url']['query'][0]['description'], 'Query Id.')

    def test_request_parameter_required(self):
        request = self._get_query_param_request()
        self.assertEqual(request['url']['raw'], '{{baseUrl}}/users/')
        # first query parameter
        self.assertEqual(request['url']['query'][0]['disabled'], False)

    def test_request_header(self):
        request = self._get_query_param_request()
        self.assertEqual(request['url']['raw'], '{{baseUrl}}/users/')
        # headers
        self.assertEqual(request['header'][0]['key'], 'Accept')
        self.assertEqual(request['header'][0]['disabled'], False)
        self.assertEqual(request['header'][1]['key'], 'Custom-Header')
        self.assertEqual(request['header'][1]['disabled'], True)
        self.assertEqual(request['header'][2]['key'], 'Another-Custom-Header')
        self.assertEqual(request['header'][2]['disabled'], False)


if __name__ == '__main__':
    unittest.main()
