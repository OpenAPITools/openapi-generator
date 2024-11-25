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

    def test_security_schemes(self):
        # auth
        self.assertEqual(self.json_data['auth']['type'], 'basic')

        self.assertEqual(self.json_data['auth']['basic'][0]['key'], 'username')
        self.assertEqual(self.json_data['auth']['basic'][0]['value'], '{{USERNAME}}')
        self.assertEqual(self.json_data['auth']['basic'][0]['type'], 'string')

        self.assertEqual(self.json_data['auth']['basic'][1]['key'], 'password')
        self.assertEqual(self.json_data['auth']['basic'][1]['value'], '{{PASSWORD}}')
        self.assertEqual(self.json_data['auth']['basic'][1]['type'], 'string')


if __name__ == '__main__':
    unittest.main()
