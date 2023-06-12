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

import petstore_api


class TestConfiguration(unittest.TestCase):
    """Animal unit test stubs"""

    def setUp(self):
        pass

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

if __name__ == '__main__':
    unittest.main()
