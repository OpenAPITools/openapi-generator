# coding: utf-8

from __future__ import absolute_import
import unittest

from flask import json
from six import BytesIO

from openapi_server.models.api_response import ApiResponse  # noqa: E501
from openapi_server.models.pet import Pet  # noqa: E501
from openapi_server.models.pet_form import PetForm  # noqa: E501
from openapi_server.models.status_enum import StatusEnum  # noqa: E501
from openapi_server.models.upload_form import UploadForm  # noqa: E501
from openapi_server.test import BaseTestCase


class TestPetController(BaseTestCase):
    """PetController integration test stubs"""

    @unittest.skip("Connexion does not support multiple consummes. See https://github.com/zalando/connexion/pull/760")
    def test_add_pet(self):
        """Test case for add_pet

        Add a new pet to the store
        """
        pet = {
  "photoUrls" : [ "photoUrls", "photoUrls" ],
  "name" : "doggie",
  "id" : 0,
  "category" : {
    "name" : "name",
    "id" : 6
  },
  "tags" : [ {
    "name" : "name",
    "id" : 1
  }, {
    "name" : "name",
    "id" : 1
  } ],
  "status" : "available"
}
        headers = { 
            'Content-Type': 'application/json',
            'Authorization': 'Bearer special-key',
        }
        response = self.client.open(
            '/v2/pet',
            method='POST',
            headers=headers,
            data=json.dumps(pet),
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_delete_pet(self):
        """Test case for delete_pet

        Deletes a pet
        """
        headers = { 
            'api_key': 'api_key_example',
            'Authorization': 'Bearer special-key',
        }
        response = self.client.open(
            '/v2/pet/{pet_id}'.format(pet_id=789),
            method='DELETE',
            headers=headers)
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_find_pets_by_status(self):
        """Test case for find_pets_by_status

        Finds Pets by status
        """
        query_string = [('status', 'available')]
        headers = { 
            'Accept': 'application/json',
            'Authorization': 'Bearer special-key',
        }
        response = self.client.open(
            '/v2/pet/findByStatus',
            method='GET',
            headers=headers,
            query_string=query_string)
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_find_pets_by_tags(self):
        """Test case for find_pets_by_tags

        Finds Pets by tags
        """
        query_string = [('tags', 'tags_example')]
        headers = { 
            'Accept': 'application/json',
            'Authorization': 'Bearer special-key',
        }
        response = self.client.open(
            '/v2/pet/findByTags',
            method='GET',
            headers=headers,
            query_string=query_string)
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_get_pet_by_id(self):
        """Test case for get_pet_by_id

        Find pet by ID
        """
        headers = { 
            'Accept': 'application/json',
            'api_key': 'special-key',
        }
        response = self.client.open(
            '/v2/pet/{pet_id}'.format(pet_id=789),
            method='GET',
            headers=headers)
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    @unittest.skip("Connexion does not support multiple consummes. See https://github.com/zalando/connexion/pull/760")
    def test_update_pet(self):
        """Test case for update_pet

        Update an existing pet
        """
        pet = {
  "photoUrls" : [ "photoUrls", "photoUrls" ],
  "name" : "doggie",
  "id" : 0,
  "category" : {
    "name" : "name",
    "id" : 6
  },
  "tags" : [ {
    "name" : "name",
    "id" : 1
  }, {
    "name" : "name",
    "id" : 1
  } ],
  "status" : "available"
}
        headers = { 
            'Content-Type': 'application/json',
            'Authorization': 'Bearer special-key',
        }
        response = self.client.open(
            '/v2/pet',
            method='PUT',
            headers=headers,
            data=json.dumps(pet),
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_update_pet_status_with_enum(self):
        """Test case for update_pet_status_with_enum

        Set the status of a pet in the store using an enum
        """
        query_string = [('status', pending)]
        headers = { 
            'Accept': 'application/json',
        }
        response = self.client.open(
            '/v2/pet/{pet_id}'.format(pet_id=789),
            method='PATCH',
            headers=headers,
            query_string=query_string)
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    @unittest.skip("application/x-www-form-urlencoded not supported by Connexion")
    def test_update_pet_with_form(self):
        """Test case for update_pet_with_form

        Updates a pet in the store with form data
        """
        pet_form = {"name":"fluffy","status":"available"}
        headers = { 
            'Content-Type': 'application/x-www-form-urlencoded',
            'Authorization': 'Bearer special-key',
        }
        response = self.client.open(
            '/v2/pet/{pet_id}'.format(pet_id=789),
            method='POST',
            headers=headers,
            data=json.dumps(pet_form),
            content_type='application/x-www-form-urlencoded')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    @unittest.skip("multipart/form-data not supported by Connexion")
    def test_upload_file(self):
        """Test case for upload_file

        uploads an image
        """
        upload_form = {"additionalMetadata":"additional metadata example","file":"c29tZSB0ZXN0IGRhdGEK"}
        headers = { 
            'Accept': 'application/json',
            'Content-Type': 'multipart/form-data',
            'Authorization': 'Bearer special-key',
        }
        response = self.client.open(
            '/v2/pet/{pet_id}/uploadImage'.format(pet_id=789),
            method='POST',
            headers=headers,
            data=json.dumps(upload_form),
            content_type='multipart/form-data')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))


if __name__ == '__main__':
    unittest.main()
