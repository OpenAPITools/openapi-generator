# coding: utf-8

# flake8: noqa

"""
Run the tests.
$ pip install -U pytest
$ cd petstore_api-python
$ pytest
"""
from __future__ import absolute_import

import unittest
from unittest.mock import patch

import petstore_api


class TestConfiguration(unittest.TestCase):
    """Animal unit test stubs"""

    def setUp(self):
        self.config = petstore_api.Configuration()

    def tearDown(self):
        # reset Configuration
        petstore_api.Configuration.set_default(None)

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
        petstore_api.Configuration.set_default(c1)

        # get default configuration
        c2 = petstore_api.Configuration.get_default_copy()
        self.assertEqual(c2.host, "example.com")
        self.assertTrue(c2.debug)

        self.assertEqual(id(c1), id(c2))
        self.assertEqual(id(c1.api_key), id(c2.api_key))
        self.assertEqual(id(c1.api_key_prefix), id(c2.api_key_prefix))

    def testApiClientDefaultConfiguration(self):
        # ensure the default configuration is the same
        p1 = petstore_api.PetApi()
        p2 = petstore_api.PetApi()
        self.assertEqual(id(p1.api_client.configuration), id(p2.api_client.configuration))

    def testAccessTokenWhenConstructingConfiguration(self):
        c1 = petstore_api.Configuration(access_token="12345")
        self.assertEqual(c1.access_token, "12345")

    def test_proxy_settings_default_from_environment(self):
        environment_proxies = {
            "http": "http://plain-proxy.example",
            "https": "http://secure-proxy.example",
            "no": "internal.example",
        }
        with patch(
            "petstore_api.configuration.getproxies",
            return_value=environment_proxies,
        ):
            config = petstore_api.Configuration(host="https://api.example")

        self.assertEqual(config.proxy, "http://secure-proxy.example")
        self.assertEqual(config.no_proxy, "internal.example")

    def test_explicit_proxy_settings_override_environment(self):
        with patch(
            "petstore_api.configuration.getproxies",
            return_value={"https": "http://proxy.example", "no": "example"},
        ) as getproxies:
            config = petstore_api.Configuration(
                host="https://api.example",
                proxy="",
                no_proxy="",
            )

        getproxies.assert_not_called()
        self.assertEqual(config.proxy, "")
        self.assertEqual(config.no_proxy, "")

    def test_explicit_proxy_does_not_resolve_generated_host(self):
        with patch(
            "petstore_api.configuration.getproxies",
            return_value={},
        ):
            config = petstore_api.Configuration(
                server_index=999,
                proxy="http://proxy.example",
            )

        self.assertEqual(config.proxy, "http://proxy.example")

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

    def testDeepcopyPreservesDebugValue(self):
        """Verify deepcopy preserves debug=True without calling the setter."""
        import copy
        c1 = petstore_api.Configuration()
        c1.debug = True
        c2 = copy.deepcopy(c1)
        self.assertTrue(c2.debug)

    def testDeepcopyPreservesDebugFalse(self):
        """Verify deepcopy preserves debug=False (the default)."""
        import copy
        c1 = petstore_api.Configuration()
        c2 = copy.deepcopy(c1)
        self.assertFalse(c2.debug)

if __name__ == '__main__':
    unittest.main()
