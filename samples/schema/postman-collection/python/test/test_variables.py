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
        # variable
        variable = self.json_data['variable']

        self.assertEqual(len(variable), 4)

        self.assertEqual(variable[0]['key'], 'baseUrl')
        self.assertEqual(variable[0]['value'], 'http://localhost:{port}/{version}')
        self.assertEqual(variable[0]['type'], 'string')


if __name__ == '__main__':
    unittest.main()
