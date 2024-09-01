"""
    Testing GoServer generator output
"""
from __future__ import absolute_import

import unittest

from test.base_test import TestBase


class TestRouters(TestBase):

    def setUp(self):
        self.file_path = './go/routers.go'
        super().setUp()

    def tearDown(self):
        pass

    def test_route_struct_exists(self):
        expected_struct = (
            "type Route struct {\n"
            "\tMethod\t  string\n"
            "\tPattern\t string\n"
            "\tHandlerFunc http.HandlerFunc\n"
            "}"
        )
        self.assertIn(expected_struct, self.file_content,
                      f"type Route was not found in {self.file_path}")

    def test_routes_map(self):
        self.assertEqual(self.lines[34], 'type Routes map[string]Route')


if __name__ == '__main__':
    unittest.main()
