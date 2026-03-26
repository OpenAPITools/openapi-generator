# coding: utf-8

# flake8: noqa

import unittest
import weakref

import petstore_api

HOST = 'http://localhost/v2'

class TestApiClient(unittest.IsolatedAsyncioTestCase):
    async def test_ignore_operation_servers(self):
        config = petstore_api.Configuration(host=HOST)
        async with petstore_api.ApiClient(config) as client:
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
