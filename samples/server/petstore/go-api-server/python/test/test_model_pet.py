"""
    Testing GoServer generator output
"""
from __future__ import absolute_import

import unittest

from test.base_test import TestBase


class TestModelPet(TestBase):

    def setUp(self):
        self.file_path = './go/model_pet.go'
        super().setUp()

    def tearDown(self):
        pass

    def test_model_id(self):
        self.assertEqual(self.lines[18], 'Id int64 `json:"id,omitempty"`')


if __name__ == '__main__':
    unittest.main()
