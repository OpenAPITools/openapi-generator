# coding: utf-8

from __future__ import absolute_import

from flask import json
from six import BytesIO

from app.openapi_server.models.api_response import ApiResponse  # noqa: E501
from app.openapi_server.models.pet import Pet  # noqa: E501
from openapi_server.test import BaseTestCase


class TestPetController(BaseTestCase):
    """PetController integration test stubs"""

    def test_add_pet(self):
        """Test case for add_pet

        Add a new pet to the store
        """
        body = {
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
        response = self.client.open(
            '/v2/pet',
            method='POST',
            data=json.dumps(body),
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_delete_pet(self):
        """Test case for delete_pet

        Deletes a pet
        """
        headers = [('api_key', 'api_key_example')]
        response = self.client.open(
            '/v2/pet/{pet_id}'.format(pet_id=56),
            method='DELETE',
            headers=headers)
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_find_pets_by_status(self):
        """Test case for find_pets_by_status

        Finds Pets by status
        """
        query_string = [('status', 'available')]
        response = self.client.open(
            '/v2/pet/findByStatus',
            method='GET',
            query_string=query_string)
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_find_pets_by_tags(self):
        """Test case for find_pets_by_tags

        Finds Pets by tags
        """
        query_string = [('tags', 'tags_example')]
        response = self.client.open(
            '/v2/pet/findByTags',
            method='GET',
            query_string=query_string)
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_get_pet_by_id(self):
        """Test case for get_pet_by_id

        Find pet by ID
        """
        response = self.client.open(
            '/v2/pet/{pet_id}'.format(pet_id=56),
            method='GET')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_update_pet(self):
        """Test case for update_pet

        Update an existing pet
        """
        body = {
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
        response = self.client.open(
            '/v2/pet',
            method='PUT',
            data=json.dumps(body),
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_update_pet_with_form(self):
        """Test case for update_pet_with_form

        Updates a pet in the store with form data
        """
        data = dict(name='name_example',
                    status='status_example')
        response = self.client.open(
            '/v2/pet/{pet_id}'.format(pet_id=56),
            method='POST',
            data=data,
            content_type='application/x-www-form-urlencoded')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_upload_file(self):
        """Test case for upload_file

        uploads an image
        """
        data = dict(additional_metadata='additional_metadata_example',
                    file=(BytesIO(b'some file data'), 'file.txt'))
        response = self.client.open(
            '/v2/pet/{pet_id}/uploadImage'.format(pet_id=56),
            method='POST',
            data=data,
            content_type='multipart/form-data')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))


if __name__ == '__main__':
    import unittest
    unittest.main()
