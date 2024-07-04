"""
    Testing Postman generator output
"""
from __future__ import absolute_import

import unittest

import json


class TestInfo(unittest.TestCase):

    def setUp(self):
        with open('./postman.json', 'r') as file:
            self.json_data = json.load(file)

    def tearDown(self):
        pass

    def test_collection_name(self):
        self.assertEqual(self.json_data['info']['name'], 'Sample project')


if __name__ == '__main__':
    unittest.main()
