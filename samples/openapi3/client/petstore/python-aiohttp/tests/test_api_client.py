# coding: utf-8

# flake8: noqa

import unittest
import weakref

from tests.util import async_test
import petstore_api

HOST = 'http://localhost/v2'

class TestApiClient(unittest.TestCase):

    @async_test
    async def test_context_manager_closes_client(self):

        async with petstore_api.ApiClient() as client:
            # pool_manager
            self.assertFalse(client.rest_client.pool_manager.closed)
            rest_pool_ref = client.rest_client.pool_manager

        self.assertTrue(rest_pool_ref.closed)

    @async_test
    async def test_ignore_operation_servers(self):
        config = petstore_api.Configuration(host=HOST)
        client = petstore_api.ApiClient(config)
        user_api_instance = petstore_api.api.user_api.UserApi(client)

        config_ignore = petstore_api.Configuration(host=HOST, ignore_operation_servers=True)
        client_ignore = petstore_api.ApiClient(config_ignore)
        user_api_instance_ignore = petstore_api.api.user_api.UserApi(client_ignore)

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
        self.assertEqual(url, 'http://petstore.swagger.io/v2/user')

        # operation servers should be ignored
        _, url_ignore, *_ = user_api_instance_ignore._create_user_serialize(**params_to_serialize)
        self.assertEqual(client.configuration.host, HOST)
        self.assertEqual(url_ignore, HOST + '/user')
