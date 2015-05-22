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

HOST = 'http://petstore.swagger.io/v2'


class ApiClientTests(unittest.TestCase):

    def setUp(self):
        self.api_client = SwaggerPetstore.ApiClient(HOST)

    def test_select_header_accept(self):
        accepts = ['application/json', 'application/xml']
        accept = SwaggerPetstore.ApiClient.select_header_accept(accepts)
        self.assertEqual(accept, 'application/json')

        accepts = ['application/xml', 'application/json']
        accept = SwaggerPetstore.ApiClient.select_header_accept(accepts)
        self.assertEqual(accept, 'application/json')

        accepts = ['text/plain', 'application/xml']
        accept = SwaggerPetstore.ApiClient.select_header_accept(accepts)
        self.assertEqual(accept, 'text/plain, application/xml')

        accepts = []
        accept = SwaggerPetstore.ApiClient.select_header_accept(accepts)
        self.assertEqual(accept, None)

    def test_select_header_content_type(self):
        content_types = ['application/json', 'application/xml']
        content_type = SwaggerPetstore.ApiClient.select_header_content_type(content_types)
        self.assertEqual(content_type, 'application/json')
        
        content_types = ['application/xml', 'application/json']
        content_type = SwaggerPetstore.ApiClient.select_header_content_type(content_types)
        self.assertEqual(content_type, 'application/json')
        
        content_types = ['text/plain', 'application/xml']
        content_type = SwaggerPetstore.ApiClient.select_header_content_type(content_types)
        self.assertEqual(content_type, 'text/plain')
        
        content_types = []
        content_type = SwaggerPetstore.ApiClient.select_header_content_type(content_types)
        self.assertEqual(content_type, 'application/json')
        

