# coding: utf-8

# flake8: noqa

import unittest
import weakref

from tests.util import async_test
import petstore_api


class TestApiClient(unittest.TestCase):

    @async_test
    async def test_context_manager_closes_client(self):

        async with petstore_api.ApiClient() as client:
            # thread pool
            self.assertIsNotNone(client.pool)
            pool_ref = weakref.ref(client._pool)
            self.assertIsNotNone(pool_ref())
            # pool_manager
            self.assertFalse(client.rest_client.pool_manager.closed)
            rest_pool_ref = client.rest_client.pool_manager

        self.assertIsNone(pool_ref())
        self.assertTrue(rest_pool_ref.closed)
