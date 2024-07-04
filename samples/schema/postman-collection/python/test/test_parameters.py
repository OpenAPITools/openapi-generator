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

    def test_request_parameter_description(self):
        # request url
        request = self.json_data['item'][2]['item'][1]['item'][0]['request']
        self.assertEqual(request['url']['raw'], '{{baseUrl}}/users/')
        # first query parameter
        self.assertEqual(request['url']['query'][0]['key'], 'pUserId')
        self.assertEqual(request['url']['query'][0]['value'], '888')
        self.assertEqual(request['url']['query'][0]['description'], 'Query Id.')

    def test_request_parameter_required(self):
        # request url
        request = self.json_data['item'][2]['item'][1]['item'][0]['request']
        self.assertEqual(request['url']['raw'], '{{baseUrl}}/users/')
        # first query parameter
        self.assertEqual(request['url']['query'][0]['disabled'], False)

    def test_request_header(self):
        # request url
        request = self.json_data['item'][2]['item'][1]['item'][0]['request']
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
