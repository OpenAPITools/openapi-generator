"""
    Testing GoServer generator output
"""
from __future__ import absolute_import

import unittest

from test.base_test import TestBase


class TestGoMod(TestBase):

    def setUp(self):
        self.file_path = './go.mod'
        super().setUp()

    def tearDown(self):
        pass

    def test_module_name(self):
        self.assertEqual(self.lines[0], 'module github.com/GIT_USER_ID/GIT_REPO_ID')

    def test_go_version(self):
        self.assertEqual(self.lines[2], 'go 1.18')


if __name__ == '__main__':
    unittest.main()
