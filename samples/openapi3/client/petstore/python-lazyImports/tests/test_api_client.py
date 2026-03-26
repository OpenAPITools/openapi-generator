# coding: utf-8

# flake8: noqa

"""
Run the tests.
$ pip install -U pytest
$ cd OpenAPIetstore-python
$ pytest
"""

import unittest
from decimal import Decimal
from enum import Enum

from dateutil.parser import parse

import petstore_api
import petstore_api.configuration

HOST = 'http://localhost/v2'


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

    def test_ignore_operation_servers(self):
        config = petstore_api.Configuration(host=HOST)
        client = petstore_api.ApiClient(config)
        user_api_instance = petstore_api.UserApi(client)

        config_ignore = petstore_api.Configuration(host=HOST, ignore_operation_servers=True)
        client_ignore = petstore_api.ApiClient(config_ignore)
        user_api_instance_ignore = petstore_api.UserApi(client_ignore)

        params_to_serialize = {
            'user': petstore_api.User(id=1, username='test'),
            '_request_auth': None,
            '_content_type': 'application/json',
            '_headers': None,
            '_host_index': 0
        }

        # operation servers should be used
        _, url, *_ = user_api_instance._create_user_serialize(**params_to_serialize)
        self.assertEqual(client.configuration.host, HOST)
        self.assertEqual(url, 'http://localhost/v2/user')

        # operation servers should be ignored
        _, url_ignore, *_ = user_api_instance_ignore._create_user_serialize(**params_to_serialize)
        self.assertEqual(client.configuration.host, HOST)
        self.assertEqual(url_ignore, HOST + '/user')

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

    def test_sanitize_for_serialization_none(self):
        data = None
        result = self.api_client.sanitize_for_serialization(None)
        self.assertEqual(result, data)

    def test_sanitize_for_serialization_str(self):
        data = "test string"
        result = self.api_client.sanitize_for_serialization(data)
        self.assertEqual(result, data)

    def test_sanitize_for_serialization_int(self):
        data = 1
        result = self.api_client.sanitize_for_serialization(data)
        self.assertEqual(result, data)

    def test_sanitize_for_serialization_bool(self):
        data = True
        result = self.api_client.sanitize_for_serialization(data)
        self.assertEqual(result, data)

    def test_sanitize_for_serialization_date(self):
        data = parse("1997-07-16").date()  # date
        result = self.api_client.sanitize_for_serialization(data)
        self.assertEqual(result, "1997-07-16")

    def test_sanitize_for_serialization_datetime(self):
        data = parse("1997-07-16T19:20:30.45+01:00")  # datetime
        result = self.api_client.sanitize_for_serialization(data)
        self.assertEqual(result, "1997-07-16T19:20:30.450000+01:00")

    def test_sanitize_for_serialization_decimal(self):
        data = Decimal("1.0")
        result = self.api_client.sanitize_for_serialization(data)
        self.assertEqual(result, "1.0")

    def test_sanitize_for_serialization_list_enum(self):
        class EnumSerialization(int, Enum):
            NUMBER_0 = 0
            NUMBER_1 = 1

        data = [EnumSerialization.NUMBER_1]
        result = self.api_client.sanitize_for_serialization(data)
        self.assertEqual(result, [1])
        self.assertNotIsInstance(result[0], EnumSerialization)

    def test_sanitize_for_serialization_list(self):
        data = [1]
        result = self.api_client.sanitize_for_serialization(data)
        self.assertEqual(result, data)

    def test_sanitize_for_serialization_dict(self):
        data = {"test key": "test value"}
        result = self.api_client.sanitize_for_serialization(data)
        self.assertEqual(result, data)

    def test_sanitize_for_serialization_model(self):
        pet_dict = {"id": 1, "name": "monkey",
                    "category": {"id": 1, "name": "test category"},
                    "tags": [{"id": 1, "name": "test tag1"},
                             {"id": 2, "name": "test tag2"}],
                    "status": "available",
                    "photoUrls": ["http://foo.bar.com/3",
                                  "http://foo.bar.com/4"]}
        pet = petstore_api.Pet(name="monkey", photoUrls=["http://foo.bar.com/3", "http://foo.bar.com/4"])
        pet.id = 1
        cate = petstore_api.Category(name="test category")
        cate.id = 1
        pet.category = cate
        tag1 = petstore_api.Tag()
        tag1.id = 1
        tag1.name = "test tag1"
        tag2 = petstore_api.Tag()
        tag2.id = 2
        tag2.name = "test tag2"
        pet.tags = [tag1, tag2]
        pet.status = "available"

        data = pet
        result = self.api_client.sanitize_for_serialization(data)
        self.assertEqual(result, pet_dict)

        # list of models
        list_of_pet_dict = [pet_dict]
        result = self.api_client.sanitize_for_serialization([pet])
        self.assertEqual(result, list_of_pet_dict)

    def test_parameters_to_url_query_simple_values(self):
        data = 'value={"category": "example", "category2": "example2"}'
        dictionary = {
            "category": "example",
            "category2": "example2"
        }
        result = self.api_client.parameters_to_url_query([('value', dictionary)], {})
        self.assertEqual(result,
                         "value=%7B%22category%22%3A%20%22example%22%2C%20%22category2%22%3A%20%22example2%22%7D")

    def test_parameters_to_url_query_complex_values(self):
        data = 'value={"number": 1, "string": "str", "bool": true, "dict": {"number": 1, "string": "str", "bool": true}}'
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
        self.assertEqual(result,
                         'value=%7B%22number%22%3A%201%2C%20%22string%22%3A%20%22str%22%2C%20%22bool%22%3A%20true%2C%20%22dict%22%3A%20%7B%22number%22%3A%201%2C%20%22string%22%3A%20%22str%22%2C%20%22bool%22%3A%20true%7D%7D')

    def test_parameters_to_url_query_dict_values(self):
        data = 'value={"strValues": ["one", "two", "three"], "dictValues": [{"name": "value1", "age": 14}, {"name": "value2", "age": 12}]}'
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
        self.assertEqual(result,
                         'value=%7B%22strValues%22%3A%20%5B%22one%22%2C%20%22two%22%2C%20%22three%22%5D%2C%20%22dictValues%22%3A%20%5B%7B%22name%22%3A%20%22value1%22%2C%20%22age%22%3A%2014%7D%2C%20%7B%22name%22%3A%20%22value2%22%2C%20%22age%22%3A%2012%7D%5D%7D')

    def test_parameters_to_url_query_boolean_value(self):
        result = self.api_client.parameters_to_url_query([('boolean', True)], {})
        self.assertEqual(result, "boolean=true")

    def test_parameters_to_url_query_list_value(self):
        params = self.api_client.parameters_to_url_query(params=[('list', [1, 2, 3])],
                                                         collection_formats={'list': 'multi'})
        self.assertEqual(params, "list=1&list=2&list=3")

    def test_parameters_to_url_query_list_value_encoded(self):
        params = self.api_client.parameters_to_url_query(params=[('list', [" !\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~", "2023-01-01T00:00:00+01:00"])],
                                                         collection_formats={'list': 'multi'})
        self.assertEqual(params, "list=%20%21%22%23%24%25%26%27%28%29%2A%2B%2C-./%3A%3B%3C%3D%3E%3F%40%5B%5C%5D%5E_%60%7B%7C%7D~&list=2023-01-01T00%3A00%3A00%2B01%3A00")
