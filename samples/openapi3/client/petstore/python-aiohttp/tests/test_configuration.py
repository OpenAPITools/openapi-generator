# coding: utf-8

# flake8: noqa

import unittest

import aiohttp
import aiohttp_retry
import petstore_api
from multidict import CIMultiDict, CIMultiDictProxy


class TestConfiguration(unittest.IsolatedAsyncioTestCase):
    def tearDown(self):
        petstore_api.Configuration.set_default(None)
        petstore_api.ApiClient.set_default(None)

    async def testCopiesPreserveTransportObjects(self):
        cookie_jar = aiohttp.DummyCookieJar()
        trace_config = aiohttp.TraceConfig()
        retries = aiohttp_retry.ExponentialRetry()
        proxy_headers = {"X-Test": "value"}
        configuration = petstore_api.Configuration(
            retries=retries,
            trace_configs=[trace_config],
            client_session_kwargs={"cookie_jar": cookie_jar},
            proxy_headers=proxy_headers,
        )

        petstore_api.Configuration.set_default(configuration)
        stored = petstore_api.Configuration.get_default()
        copied = petstore_api.Configuration.get_default_copy()

        self.assertIsNot(stored.client_session_kwargs,
                         configuration.client_session_kwargs)
        self.assertIsNot(copied.client_session_kwargs,
                         stored.client_session_kwargs)
        assert copied.client_session_kwargs is not None
        self.assertIs(copied.client_session_kwargs["cookie_jar"], cookie_jar)
        self.assertIsNot(stored.trace_configs, configuration.trace_configs)
        self.assertIsNot(copied.trace_configs, stored.trace_configs)
        assert copied.trace_configs is not None
        self.assertIs(copied.trace_configs[0], trace_config)
        self.assertIs(copied.retries, retries)
        assert stored.proxy_headers is not None
        assert copied.proxy_headers is not None
        self.assertIsNot(stored.proxy_headers, proxy_headers)
        self.assertIsNot(copied.proxy_headers, stored.proxy_headers)
        proxy_headers["X-Test"] = "source mutation"
        stored.proxy_headers["X-Test"] = "stored mutation"
        self.assertEqual(copied.proxy_headers, {"X-Test": "value"})

    async def testCopiesProxyHeaderMultidict(self):
        source_headers = CIMultiDict([
            ("X-Test", "first"),
            ("X-Test", "second"),
        ])
        proxy_headers = CIMultiDictProxy(source_headers)
        configuration = petstore_api.Configuration(
            proxy_headers=proxy_headers,
        )

        petstore_api.Configuration.set_default(configuration)
        stored = petstore_api.Configuration.get_default()
        copied = petstore_api.Configuration.get_default_copy()

        assert stored.proxy_headers is not None
        assert copied.proxy_headers is not None
        self.assertIsNot(stored.proxy_headers, proxy_headers)
        self.assertIsNot(copied.proxy_headers, stored.proxy_headers)
        source_headers.add("X-Test", "source mutation")
        stored.proxy_headers.add("X-Test", "stored mutation")
        self.assertEqual(proxy_headers.getall("X-Test"),
                         ["first", "second", "source mutation"])
        self.assertEqual(stored.proxy_headers.getall("X-Test"),
                         ["first", "second", "stored mutation"])
        self.assertEqual(copied.proxy_headers.getall("X-Test"),
                         ["first", "second"])

    async def testImplicitApiClientLifecycle(self):
        implicit_api = petstore_api.PetApi()
        implicit_rest_client = implicit_api.api_client.rest_client
        implicit_session = implicit_rest_client._create_pool_manager()
        implicit_rest_client.pool_manager = implicit_session

        async with implicit_api as entered:
            self.assertIs(entered, implicit_api)

        self.assertTrue(implicit_session.closed)

        explicit_client = petstore_api.ApiClient()
        explicit_rest_client = explicit_client.rest_client
        explicit_session = explicit_rest_client._create_pool_manager()
        explicit_rest_client.pool_manager = explicit_session
        try:
            async with petstore_api.PetApi(explicit_client):
                pass

            self.assertFalse(explicit_session.closed)
        finally:
            await explicit_client.close()

        default_client = petstore_api.ApiClient()
        default_rest_client = default_client.rest_client
        default_session = default_rest_client._create_pool_manager()
        default_rest_client.pool_manager = default_session
        petstore_api.ApiClient.set_default(default_client)
        try:
            async with petstore_api.PetApi() as default_api:
                self.assertIs(default_api.api_client, default_client)

            self.assertFalse(default_session.closed)
        finally:
            await default_client.close()

    async def testSuppliedConnectorRemainsCallerOwned(self):
        connector = aiohttp.TCPConnector()
        configuration = petstore_api.Configuration(
            client_session_kwargs={
                "connector": connector,
                "connector_owner": True,
            },
        )
        petstore_api.Configuration.set_default(configuration)
        first_api = petstore_api.PetApi()
        second_api = petstore_api.PetApi()
        first_rest_client = first_api.api_client.rest_client
        second_rest_client = second_api.api_client.rest_client
        first_session = first_rest_client._create_pool_manager()
        second_session = second_rest_client._create_pool_manager()
        first_rest_client.pool_manager = first_session
        second_rest_client.pool_manager = second_session

        try:
            self.assertIs(first_session.connector, connector)
            self.assertIs(second_session.connector, connector)
            self.assertFalse(first_session.connector_owner)
            self.assertFalse(second_session.connector_owner)

            await first_api.close()
            self.assertTrue(first_session.closed)
            self.assertFalse(second_session.closed)
            self.assertFalse(connector.closed)
            await second_api.close()
            self.assertTrue(second_session.closed)
            self.assertFalse(connector.closed)
        finally:
            await first_api.close()
            await second_api.close()
            await connector.close()

    async def testNoneConnectorRemainsSessionOwned(self):
        configuration = petstore_api.Configuration(
            client_session_kwargs={
                "connector": None,
                "connector_owner": False,
            },
        )
        petstore_api.Configuration.set_default(configuration)
        api = petstore_api.PetApi()
        rest_client = api.api_client.rest_client
        session = rest_client._create_pool_manager()
        rest_client.pool_manager = session
        connector = session.connector

        try:
            self.assertTrue(session.connector_owner)
            self.assertIsNotNone(connector)
            await api.close()
            self.assertTrue(session.closed)
            self.assertTrue(connector.closed)
        finally:
            await api.close()

    async def testImplicitApiClientClosesOwnedClientAfterReassignment(self):
        implicit_api = petstore_api.PetApi()
        owned_rest_client = implicit_api.api_client.rest_client
        owned_session = owned_rest_client._create_pool_manager()
        owned_rest_client.pool_manager = owned_session
        replacement_client = petstore_api.ApiClient()
        replacement_rest_client = replacement_client.rest_client
        replacement_session = replacement_rest_client._create_pool_manager()
        replacement_rest_client.pool_manager = replacement_session
        implicit_api.api_client = replacement_client

        try:
            await implicit_api.close()

            self.assertTrue(owned_session.closed)
            self.assertFalse(replacement_session.closed)
        finally:
            await replacement_client.close()
