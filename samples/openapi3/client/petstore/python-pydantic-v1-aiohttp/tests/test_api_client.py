# coding: utf-8

# flake8: noqa

import unittest

import petstore_api


class TestApiClient(unittest.IsolatedAsyncioTestCase):
    async def test_context_manager_closes_client(self):
        async with petstore_api.ApiClient() as client:
            # pool_manager
            self.assertFalse(client.rest_client.pool_manager.closed)
            rest_pool_ref = client.rest_client.pool_manager

        self.assertTrue(rest_pool_ref.closed)
