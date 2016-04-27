# coding: utf-8

"""
Copyright 2016 SmartBear Software

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.

   ref: https://github.com/swagger-api/swagger-codegen
"""

from __future__ import absolute_import

import os
import sys
import unittest

import swagger_client
from swagger_client.rest import ApiException
from swagger_client.apis.pet_api import PetApi


class TestPetApi(unittest.TestCase):
    """ PetApi unit test stubs """

    def setUp(self):
        self.api = swagger_client.apis.pet_api.PetApi()

    def tearDown(self):
        pass

    def test_add_pet(self):
        """
        Test case for add_pet

        Add a new pet to the store
        """
        pass

    def test_delete_pet(self):
        """
        Test case for delete_pet

        Deletes a pet
        """
        pass

    def test_find_pets_by_status(self):
        """
        Test case for find_pets_by_status

        Finds Pets by status
        """
        pass

    def test_find_pets_by_tags(self):
        """
        Test case for find_pets_by_tags

        Finds Pets by tags
        """
        pass

    def test_get_pet_by_id(self):
        """
        Test case for get_pet_by_id

        Find pet by ID
        """
        pass

    def test_update_pet(self):
        """
        Test case for update_pet

        Update an existing pet
        """
        pass

    def test_update_pet_with_form(self):
        """
        Test case for update_pet_with_form

        Updates a pet in the store with form data
        """
        pass

    def test_upload_file(self):
        """
        Test case for upload_file

        uploads an image
        """
        pass


if __name__ == '__main__':
    unittest.main()