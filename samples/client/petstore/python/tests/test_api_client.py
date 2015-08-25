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
from dateutil.parser import parse

import swagger_client
import swagger_client.configuration

HOST = 'http://petstore.swagger.io/v2'


class ApiClientTests(unittest.TestCase):

    def setUp(self):
        self.api_client = swagger_client.ApiClient(HOST)

    def test_configuration(self):
        swagger_client.configuration.api_key['api_key'] = '123456'
        swagger_client.configuration.api_key_prefix['api_key'] = 'PREFIX'
        swagger_client.configuration.username = 'test_username'
        swagger_client.configuration.password = 'test_password'

        header_params = {'test1': 'value1'}
        query_params = {'test2': 'value2'}
        auth_settings = ['api_key', 'unknown']

        # test prefix
        self.assertEqual('PREFIX', swagger_client.configuration.api_key_prefix['api_key'])

        # update parameters based on auth setting
        self.api_client.update_params_for_auth(header_params, query_params, auth_settings)

        # test api key auth
        self.assertEqual(header_params['test1'], 'value1')
        self.assertEqual(header_params['api_key'], 'PREFIX 123456')
        self.assertEqual(query_params['test2'], 'value2')

        # test basic auth
        self.assertEqual('test_username', swagger_client.configuration.username)
        self.assertEqual('test_password', swagger_client.configuration.password)

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

    def test_sanitize_for_serialization(self):
        # None
        data = None
        result = self.api_client.sanitize_for_serialization(None)
        self.assertEqual(result, data)

        # str
        data = "test string"
        result = self.api_client.sanitize_for_serialization(data)
        self.assertEqual(result, data)

        # int
        data = 1
        result = self.api_client.sanitize_for_serialization(data)
        self.assertEqual(result, data)

        # bool
        data = True
        result = self.api_client.sanitize_for_serialization(data)
        self.assertEqual(result, data)

        # date
        data = parse("1997-07-16").date()    # date
        result = self.api_client.sanitize_for_serialization(data)
        self.assertEqual(result, "1997-07-16")

        # datetime
        data = parse("1997-07-16T19:20:30.45+01:00")    # datetime
        result = self.api_client.sanitize_for_serialization(data)
        self.assertEqual(result, "1997-07-16T19:20:30.450000+01:00")

        # list
        data = [1]
        result = self.api_client.sanitize_for_serialization(data)
        self.assertEqual(result, data)

        # dict
        data = {"test key": "test value"}
        result = self.api_client.sanitize_for_serialization(data)
        self.assertEqual(result, data)

        # model
        pet_dict = {"id": 1, "name": "monkey",
                    "category": {"id": 1, "name": "test category"},
                    "tags": [{"id": 1, "name": "test tag1"},
                             {"id": 2, "name": "test tag2"}],
                    "status": "available",
                    "photoUrls": ["http://foo.bar.com/3",
                                  "http://foo.bar.com/4"]}
        pet = swagger_client.Pet()
        pet.id = pet_dict["id"]
        pet.name = pet_dict["name"]
        cate = swagger_client.Category()
        cate.id = pet_dict["category"]["id"]
        cate.name = pet_dict["category"]["name"]
        pet.category = cate
        tag1 = swagger_client.Tag()
        tag1.id = pet_dict["tags"][0]["id"]
        tag1.name = pet_dict["tags"][0]["name"]
        tag2 = swagger_client.Tag()
        tag2.id = pet_dict["tags"][1]["id"]
        tag2.name = pet_dict["tags"][1]["name"]
        pet.tags = [tag1, tag2]
        pet.status = pet_dict["status"]
        pet.photo_urls = pet_dict["photoUrls"]

        data = pet
        result = self.api_client.sanitize_for_serialization(data)
        self.assertEqual(result, pet_dict)

        # list of models
        list_of_pet_dict = [pet_dict]
        data = [pet]
        result = self.api_client.sanitize_for_serialization(data)
        self.assertEqual(result, list_of_pet_dict)
