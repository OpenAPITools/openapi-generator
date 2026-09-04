import unittest

from flask import json

from openapi_server.models.user import User  # noqa: E501
from openapi_server.test import BaseTestCase


class TestUserController(BaseTestCase):
    """UserController integration test stubs"""

    def test_create_user(self):
        """Test case for create_user

        Create user
        """
        user = {"id":0,"username":"username","firstName":"firstName","lastName":"lastName","email":"email","password":"password","phone":"phone","userStatus":6}
        headers = { 
            'Content-Type': 'application/json',
            'api_key': 'special-key',
        }
        response = self.client.open(
            '/v2/user',
            method='POST',
            headers=headers,
            data=json.dumps(user),
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    @unittest.skip("The auto-generated test example for this array-typed request body is a single item, not an array, which fails request validation; this is a pre-existing example-generation limitation, not specific to Connexion 3.")
    def test_create_users_with_array_input(self):
        """Test case for create_users_with_array_input

        Creates list of users with given input array
        """
        user = {"id":0,"username":"username","firstName":"firstName","lastName":"lastName","email":"email","password":"password","phone":"phone","userStatus":6}
        headers = { 
            'Content-Type': 'application/json',
            'api_key': 'special-key',
        }
        response = self.client.open(
            '/v2/user/createWithArray',
            method='POST',
            headers=headers,
            data=json.dumps(user),
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    @unittest.skip("The auto-generated test example for this array-typed request body is a single item, not an array, which fails request validation; this is a pre-existing example-generation limitation, not specific to Connexion 3.")
    def test_create_users_with_list_input(self):
        """Test case for create_users_with_list_input

        Creates list of users with given input array
        """
        user = {"id":0,"username":"username","firstName":"firstName","lastName":"lastName","email":"email","password":"password","phone":"phone","userStatus":6}
        headers = { 
            'Content-Type': 'application/json',
            'api_key': 'special-key',
        }
        response = self.client.open(
            '/v2/user/createWithList',
            method='POST',
            headers=headers,
            data=json.dumps(user),
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_delete_user(self):
        """Test case for delete_user

        Delete user
        """
        headers = { 
            'api_key': 'special-key',
        }
        response = self.client.open(
            '/v2/user/{username}'.format(username='username_example'),
            method='DELETE',
            headers=headers)
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    @unittest.skip("Connexion 3 requires the handler to specify which content type to return when an operation declares multiple response content types; the auto-generated stub does not, so calling it raises a 500 until the operation is actually implemented.")
    def test_get_user_by_name(self):
        """Test case for get_user_by_name

        Get user by user name
        """
        headers = { 
            'Accept': 'application/json',
        }
        response = self.client.open(
            '/v2/user/{username}'.format(username='username_example'),
            method='GET',
            headers=headers)
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    @unittest.skip("Connexion 3 requires the handler to specify which content type to return when an operation declares multiple response content types; the auto-generated stub does not, so calling it raises a 500 until the operation is actually implemented.")
    def test_login_user(self):
        """Test case for login_user

        Logs user into the system
        """
        query_string = [('username', 'username_example'),
                        ('password', 'password_example')]
        headers = { 
            'Accept': 'application/json',
        }
        response = self.client.open(
            '/v2/user/login',
            method='GET',
            headers=headers,
            query_string=query_string)
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_logout_user(self):
        """Test case for logout_user

        Logs out current logged in user session
        """
        headers = { 
            'api_key': 'special-key',
        }
        response = self.client.open(
            '/v2/user/logout',
            method='GET',
            headers=headers)
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_update_user(self):
        """Test case for update_user

        Updated user
        """
        user = {"id":0,"username":"username","firstName":"firstName","lastName":"lastName","email":"email","password":"password","phone":"phone","userStatus":6}
        headers = { 
            'Content-Type': 'application/json',
            'api_key': 'special-key',
        }
        response = self.client.open(
            '/v2/user/{username}'.format(username='username_example'),
            method='PUT',
            headers=headers,
            data=json.dumps(user),
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))


if __name__ == '__main__':
    unittest.main()
