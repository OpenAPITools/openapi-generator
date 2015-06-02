# coding: utf-8

"""
Run the tests.
$ pip install nose (optional)
$ cd SwaggerPetstore-python
$ nosetests -v
"""

import os
import time
import unittest

import SwaggerPetstore
import SwaggerPetstore.configuration

HOST = 'http://petstore.swagger.io/v2'


class ApiClientTests(unittest.TestCase):

    def setUp(self):
        self.api_client = SwaggerPetstore.ApiClient(HOST)

    def test_configuratjion(self):
        SwaggerPetstore.configuration.api_key['api_key'] = '123456'
        SwaggerPetstore.configuration.api_key_prefix['api_key'] = 'PREFIX'
        SwaggerPetstore.configuration.username = 'test_username'
        SwaggerPetstore.configuration.password = 'test_password'

        header_params = {'test1': 'value1'}
        query_params = {'test2': 'value2'}
        auth_settings = ['api_key', 'unknown']

        # test prefix
        self.assertEqual('PREFIX', SwaggerPetstore.configuration.api_key_prefix['api_key'])

        # update parameters based on auth setting
        self.api_client.update_params_for_auth(header_params, query_params, auth_settings)

        # test api key auth
        self.assertEqual(header_params['test1'], 'value1')
        self.assertEqual(header_params['api_key'], 'PREFIX 123456')
        self.assertEqual(query_params['test2'], 'value2')

        # test basic auth
        self.assertEqual('test_username', SwaggerPetstore.configuration.username)
        self.assertEqual('test_password', SwaggerPetstore.configuration.password)

    def test_select_header_accept(self):
        accepts = ['APPLICATION/JSON', 'APPLICATION/XML']
        accept = self.api_client.select_header_accept(accepts)
        self.assertEqual(accept, 'application/json')
        
        accepts = ['application/json', 'application/xml']
        accept = self.api_client.select_header_accept(accepts)
        self.assertEqual(accept, 'application/json')

        accepts = ['application/xml', 'application/json']
        accept = self.api_client.select_header_accept(accepts)
        self.assertEqual(accept, 'application/json')

        accepts = ['text/plain', 'application/xml']
        accept = self.api_client.select_header_accept(accepts)
        self.assertEqual(accept, 'text/plain, application/xml')

        accepts = []
        accept = self.api_client.select_header_accept(accepts)
        self.assertEqual(accept, None)

    def test_select_header_content_type(self):
        content_types = ['APPLICATION/JSON', 'APPLICATION/XML']
        content_type = self.api_client.select_header_content_type(content_types)
        self.assertEqual(content_type, 'application/json')
        
        content_types = ['application/json', 'application/xml']
        content_type = self.api_client.select_header_content_type(content_types)
        self.assertEqual(content_type, 'application/json')
        
        content_types = ['application/xml', 'application/json']
        content_type = self.api_client.select_header_content_type(content_types)
        self.assertEqual(content_type, 'application/json')
        
        content_types = ['text/plain', 'application/xml']
        content_type = self.api_client.select_header_content_type(content_types)
        self.assertEqual(content_type, 'text/plain')
        
        content_types = []
        content_type = self.api_client.select_header_content_type(content_types)
        self.assertEqual(content_type, 'application/json')
