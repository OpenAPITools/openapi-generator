"""
    Testing GoServer generator output
"""
from __future__ import absolute_import

import unittest


class TestBase(unittest.TestCase):

    def setUp(self):
        # load lines
        with open(self.file_path, 'r') as file:
            self.lines = file.readlines()
            self.lines = [line.strip() for line in self.lines]
        # load content
        with open(self.file_path, 'r') as file:
            self.file_content = file.read()

    def tearDown(self):
        pass


if __name__ == '__main__':
    unittest.main()
