# coding: utf-8

# flake8: noqa

"""
Run the tests.
$ pip install -U pytest
$ cd OpenAPIetstore-python
$ pytest
"""

import os
import time
import atexit
import weakref
import unittest
from dateutil.parser import parse

import petstore_api
import petstore_api.configuration

HOST = 'http://petstore.swagger.io/v2'


class ApiClientTests(unittest.TestCase):

    def setUp(self):
        self.api_client = petstore_api.ApiClient()

    def test_configuration(self):
        config = petstore_api.Configuration()

        config.api_key['api_key'] = '123456'
        config.api_key_prefix['api_key'] = 'PREFIX'
        config.username = 'test_username'
        config.password = 'test_password'

        header_params = {'test1': 'value1'}
        query_params = {'test2': 'value2'}
        auth_settings = ['api_key', 'unknown']

        client = petstore_api.ApiClient(config)

        # test prefix
        self.assertEqual('PREFIX', client.configuration.api_key_prefix['api_key'])

        # update parameters based on auth setting
        client.update_params_for_auth(header_params, query_params,
                                      auth_settings,
                                      None, None, None)

        # test api key auth
        self.assertEqual(header_params['test1'], 'value1')
        self.assertEqual(header_params['api_key'], 'PREFIX 123456')
        self.assertEqual(query_params['test2'], 'value2')

        # test basic auth
        self.assertEqual('test_username', client.configuration.username)
        self.assertEqual('test_password', client.configuration.password)

    def test_select_header_accept(self):
        accepts = ['APPLICATION/JSON', 'APPLICATION/XML']
        accept = self.api_client.select_header_accept(accepts)
        self.assertEqual(accept, 'APPLICATION/JSON')

        accepts = ['application/json', 'application/xml']
        accept = self.api_client.select_header_accept(accepts)
        self.assertEqual(accept, 'application/json')

        accepts = ['application/xml', 'application/json']
        accept = self.api_client.select_header_accept(accepts)
        self.assertEqual(accept, 'application/json')

        accepts = ['application/xml', 'application/json-patch+json']
        accept = self.api_client.select_header_accept(accepts)
        self.assertEqual(accept, 'application/json-patch+json')

        accepts = ['application/xml', 'application/json; charset=utf-8']
        accept = self.api_client.select_header_accept(accepts)
        self.assertEqual(accept, 'application/json; charset=utf-8')

        accepts = ['application/xml', 'application/json;format=flowed']
        accept = self.api_client.select_header_accept(accepts)
        self.assertEqual(accept, 'application/json;format=flowed')

        accepts = ['text/plain', 'application/xml']
        accept = self.api_client.select_header_accept(accepts)
        self.assertEqual(accept, 'text/plain')

        accepts = []
        accept = self.api_client.select_header_accept(accepts)
        self.assertEqual(accept, None)

    def test_select_header_content_type(self):
        content_types = ['APPLICATION/JSON', 'APPLICATION/XML']
        content_type = self.api_client.select_header_content_type(content_types)
        self.assertEqual(content_type, 'APPLICATION/JSON')

        content_types = ['application/json', 'application/xml']
        content_type = self.api_client.select_header_content_type(content_types)
        self.assertEqual(content_type, 'application/json')

        content_types = ['application/xml', 'application/json']
        content_type = self.api_client.select_header_content_type(content_types)
        self.assertEqual(content_type, 'application/json')

        content_types = ['application/xml', 'application/json-patch+json']
        content_type = self.api_client.select_header_content_type(content_types)
        self.assertEqual(content_type, 'application/json-patch+json')

        content_types = ['application/xml', 'application/json; charset=utf-8']
        content_type = self.api_client.select_header_content_type(content_types)
        self.assertEqual(content_type, 'application/json; charset=utf-8')

        content_types = ['application/xml', 'application/json;format=flowed']
        content_type = self.api_client.select_header_content_type(content_types)
        self.assertEqual(content_type, 'application/json;format=flowed')

        content_types = ['text/plain', 'application/xml']
        content_type = self.api_client.select_header_content_type(content_types)
        self.assertEqual(content_type, 'text/plain')

        # no content type, default to None        
        content_types = []
        content_type = self.api_client.select_header_content_type(content_types)
        self.assertEqual(content_type, None)

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
        data = parse("1997-07-16").date()  # date
        result = self.api_client.sanitize_for_serialization(data)
        self.assertEqual(result, "1997-07-16")

        # datetime
        data = parse("1997-07-16T19:20:30.45+01:00")  # datetime
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
        pet = petstore_api.Pet(name=pet_dict["name"], photo_urls=pet_dict["photoUrls"])
        pet.id = pet_dict["id"]
        cate = petstore_api.Category(name="something")
        cate.id = pet_dict["category"]["id"]
        cate.name = pet_dict["category"]["name"]
        pet.category = cate
        tag1 = petstore_api.Tag()
        tag1.id = pet_dict["tags"][0]["id"]
        tag1.name = pet_dict["tags"][0]["name"]
        tag2 = petstore_api.Tag()
        tag2.id = pet_dict["tags"][1]["id"]
        tag2.name = pet_dict["tags"][1]["name"]
        pet.tags = [tag1, tag2]
        pet.status = pet_dict["status"]

        data = pet
        result = self.api_client.sanitize_for_serialization(data)
        self.assertEqual(result, pet_dict)

        # list of models
        list_of_pet_dict = [pet_dict]
        data = [pet]
        result = self.api_client.sanitize_for_serialization(data)
        self.assertEqual(result, list_of_pet_dict)

    def test_context_manager_closes_threadpool(self):
        with petstore_api.ApiClient() as client:
            self.assertIsNotNone(client.pool)
            pool_ref = weakref.ref(client._pool)
            self.assertIsNotNone(pool_ref())
        self.assertIsNone(pool_ref())

    def test_atexit_closes_threadpool(self):
        client = petstore_api.ApiClient()
        self.assertIsNotNone(client.pool)
        self.assertIsNotNone(client._pool)
        atexit._run_exitfuncs()
        self.assertIsNone(client._pool)

    def test_parameters_to_url_query(self):
        data = 'value={"category": "example", "category2": "example2"}'
        dictionary = {
            "category": "example",
            "category2": "example2"
        }
        result = self.api_client.parameters_to_url_query([('value', dictionary)], {})
        self.assertEqual(result, "value=%7B%22category%22%3A%20%22example%22%2C%20%22category2%22%3A%20%22example2%22%7D")
        
        data='value={"number": 1, "string": "str", "bool": true, "dict": {"number": 1, "string": "str", "bool": true}}'
        dictionary = {
            "number": 1,
            "string": "str",
            "bool": True,
            "dict": {
                "number": 1,
                "string": "str",
                "bool": True
            }
        }
        result = self.api_client.parameters_to_url_query([('value', dictionary)], {})
        self.assertEqual(result, 'value=%7B%22number%22%3A%201%2C%20%22string%22%3A%20%22str%22%2C%20%22bool%22%3A%20true%2C%20%22dict%22%3A%20%7B%22number%22%3A%201%2C%20%22string%22%3A%20%22str%22%2C%20%22bool%22%3A%20true%7D%7D')

        data='value={"strValues": ["one", "two", "three"], "dictValues": [{"name": "value1", "age": 14}, {"name": "value2", "age": 12}]}'
        dictionary = {
            "strValues": [
                "one",
                "two",
                "three"
            ],
            "dictValues": [
                {
                    "name": "value1",
                    "age": 14
                },
                {
                    "name": "value2",
                    "age": 12
                },
            ]
        }
        result = self.api_client.parameters_to_url_query([('value', dictionary)], {})
        self.assertEqual(result, 'value=%7B%22strValues%22%3A%20%5B%22one%22%2C%20%22two%22%2C%20%22three%22%5D%2C%20%22dictValues%22%3A%20%5B%7B%22name%22%3A%20%22value1%22%2C%20%22age%22%3A%2014%7D%2C%20%7B%22name%22%3A%20%22value2%22%2C%20%22age%22%3A%2012%7D%5D%7D')



