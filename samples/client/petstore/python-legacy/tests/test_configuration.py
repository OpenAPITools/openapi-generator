# coding: utf-8

# flake8: noqa

"""
Run the tests.
$ pip install nose (optional)
$ cd petstore_api-python
$ nosetests -v
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

        self.assertNotEqual(id(c1.api_key), id(c2.api_key))
        self.assertNotEqual(id(c1.api_key_prefix), id(c2.api_key_prefix))


if __name__ == '__main__':
    unittest.main()
