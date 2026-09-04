# coding: utf-8

# flake8: noqa

"""
Run the tests.
$ pip install -U pytest
$ cd petstore_api-python
$ pytest
"""
from __future__ import absolute_import

import os
import tempfile
import unittest

import petstore_api


class TestConfiguration(unittest.TestCase):
    """Animal unit test stubs"""

    def setUp(self):
        self.config = petstore_api.Configuration()

    def tearDown(self):
        # reset Configuration
        petstore_api.Configuration.set_default(None)
        petstore_api.ApiClient.set_default(None)

    def testConfiguration(self):
        # check that different instances use different dictionaries
        c1 = petstore_api.Configuration()
        c2 = petstore_api.Configuration()
        self.assertNotEqual(id(c1), id(c2))
        self.assertNotEqual(id(c1.api_key), id(c2.api_key))
        self.assertNotEqual(id(c1.api_key_prefix), id(c2.api_key_prefix))

    def testDefaultConfiguration(self):
        # prepare default configuration
        c1 = petstore_api.Configuration(host="example.com")
        c1.debug = True
        c1.api_key["key"] = "value"
        petstore_api.Configuration.set_default(c1)
        c1.host = "changed.example.com"
        c1.api_key["key"] = "changed"

        # get independent copies of the registered configuration
        c2 = petstore_api.Configuration.get_default_copy()
        c3 = petstore_api.Configuration.get_default_copy()
        self.assertEqual(c2.host, "example.com")
        self.assertTrue(c2.debug)
        self.assertEqual(c2.api_key["key"], "value")

        self.assertNotEqual(id(c1), id(c2))
        self.assertNotEqual(id(c2), id(c3))
        self.assertNotEqual(id(c1.api_key), id(c2.api_key))
        self.assertNotEqual(id(c2.api_key), id(c3.api_key))
        self.assertNotEqual(id(c1.api_key_prefix), id(c2.api_key_prefix))
        self.assertNotEqual(id(c2.api_key_prefix), id(c3.api_key_prefix))

        shared1 = petstore_api.Configuration.get_default()
        shared2 = petstore_api.Configuration.get_default()
        self.assertEqual(id(shared1), id(shared2))
        self.assertNotEqual(id(c1), id(shared1))

    def testApiClientDefaultConfiguration(self):
        configuration = petstore_api.Configuration(host="example.com")
        petstore_api.Configuration.set_default(configuration)

        # each implicit client receives its own configuration copy
        p1 = petstore_api.PetApi()
        p2 = petstore_api.PetApi()
        self.assertEqual(p1.api_client.configuration.host, "example.com")
        self.assertEqual(p2.api_client.configuration.host, "example.com")
        self.assertNotEqual(id(p1.api_client), id(p2.api_client))
        self.assertNotEqual(id(p1.api_client.configuration), id(p2.api_client.configuration))

        unregistered_client = petstore_api.ApiClient.get_default()
        p3 = petstore_api.PetApi()
        self.assertIsNot(p3.api_client, unregistered_client)
        unregistered_client.close()

        default_client = petstore_api.ApiClient(
            petstore_api.Configuration(host="default-client.example.com")
        )
        petstore_api.ApiClient.set_default(default_client)
        p4 = petstore_api.PetApi()
        self.assertEqual(id(p4.api_client), id(default_client))

    def testImplicitApiClientLifecycle(self):
        implicit_api = petstore_api.PetApi()
        implicit_pool = implicit_api.api_client.rest_client.pool_manager
        implicit_pool.connection_from_url("http://example.com")
        self.assertGreater(len(implicit_pool.pools), 0)

        with implicit_api as entered:
            self.assertIs(entered, implicit_api)

        self.assertEqual(len(implicit_pool.pools), 0)

        explicit_client = petstore_api.ApiClient()
        explicit_pool = explicit_client.rest_client.pool_manager
        explicit_pool.connection_from_url("http://example.com")
        try:
            with petstore_api.PetApi(explicit_client):
                pass

            self.assertGreater(len(explicit_pool.pools), 0)
        finally:
            explicit_client.close()

    def testImplicitApiClientClosesOwnedClientAfterReassignment(self):
        implicit_api = petstore_api.PetApi()
        owned_client = implicit_api.api_client
        owned_pool = owned_client.rest_client.pool_manager
        owned_pool.connection_from_url("http://example.com")
        replacement_client = petstore_api.ApiClient()
        replacement_pool = replacement_client.rest_client.pool_manager
        replacement_pool.connection_from_url("http://example.com")
        implicit_api.api_client = replacement_client

        try:
            implicit_api.close()

            self.assertEqual(len(owned_pool.pools), 0)
            self.assertGreater(len(replacement_pool.pools), 0)
        finally:
            replacement_client.close()

    def testConfigurationCopiesReuseLoggerHandlers(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            configuration = petstore_api.Configuration()
            loggers = tuple(configuration.logger.values())
            original_handlers = {
                logger: tuple(logger.handlers) for logger in loggers
            }
            configuration.logger_file = os.path.join(temp_dir, "client.log")
            handler = configuration.logger_file_handler
            self.assertIsNotNone(handler)
            configured_handlers = {
                logger: tuple(logger.handlers) for logger in loggers
            }

            try:
                petstore_api.Configuration.set_default(configuration)
                copied = petstore_api.Configuration.get_default_copy()

                self.assertIs(copied.logger_file_handler, handler)
                for logger in loggers:
                    self.assertEqual(tuple(logger.handlers),
                                     configured_handlers[logger])
            finally:
                added_handlers = set()
                for logger in loggers:
                    for added_handler in tuple(logger.handlers):
                        if added_handler not in original_handlers[logger]:
                            logger.removeHandler(added_handler)
                            added_handlers.add(added_handler)
                for added_handler in added_handlers:
                    added_handler.close()

    def testAccessTokenWhenConstructingConfiguration(self):
        c1 = petstore_api.Configuration(access_token="12345")
        self.assertEqual(c1.access_token, "12345")

    def test_ignore_operation_servers(self):
        self.config.ignore_operation_servers = True
        self.assertTrue(self.config.ignore_operation_servers)
        self.config.ignore_operation_servers = False
        self.assertFalse(self.config.ignore_operation_servers)

        c1 = petstore_api.Configuration(ignore_operation_servers=True)
        self.assertTrue(c1.ignore_operation_servers)

        c2 = petstore_api.Configuration()
        self.assertFalse(c2.ignore_operation_servers)

    def test_get_host_settings(self):
        host_settings = self.config.get_host_settings()

        self.assertEqual('http://{server}.swagger.io:{port}/v2', host_settings[0]['url'])
        self.assertEqual('petstore', host_settings[0]['variables']['server']['default_value'])

        self.assertEqual('https://localhost:8080/{version}', host_settings[1]['url'])
        self.assertEqual('v2', host_settings[1]['variables']['version']['default_value'])

    def test_get_host_from_settings(self):
        """ Test get_host_from_settings

        Test get URL from host settings
        """
        self.assertEqual("http://petstore.swagger.io:80/v2", self.config.get_host_from_settings(0))
        self.assertEqual("http://petstore.swagger.io:8080/v2", self.config.get_host_from_settings(0, {'port': '8080'}))
        self.assertEqual("http://dev-petstore.swagger.io:8080/v2", self.config.get_host_from_settings(0, {'server': 'dev-petstore', 'port': '8080'}))

    def testConfigurationDebug(self):
        for debug, expected in [(True, True), (False, False), (None, False)]:
            with self.subTest('expicitly passing debug parameter', debug=debug, expected=expected):
                c = petstore_api.Configuration(debug=debug)
                self.assertEqual(expected, c.debug)
        with self.subTest('not passing debug parameter'):
            c = petstore_api.Configuration()
            self.assertFalse(c.debug)

if __name__ == '__main__':
    unittest.main()
