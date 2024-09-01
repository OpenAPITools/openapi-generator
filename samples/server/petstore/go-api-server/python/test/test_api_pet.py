"""
    Testing GoServer generator output
"""
from __future__ import absolute_import

import unittest

from test.base_test import TestBase


class TestApiPet(TestBase):

    def setUp(self):
        self.file_path = './go/api_pet.go'
        super().setUp()

    def tearDown(self):
        pass

    def test_route_delete_pet(self):
        expected_struct = (
            "\t\t\"DeletePet\": Route{\n"
            "\t\t\tstrings.ToUpper(\"Delete\"),\n"
            "\t\t\t\"/v2/pet/{petId}\",\n"
            "\t\t\tc.DeletePet,\n"
            "\t\t}"
        )
        self.assertIn(expected_struct, self.file_content,
                      f"type Route was not found in {self.file_path}")


if __name__ == '__main__':
    unittest.main()
