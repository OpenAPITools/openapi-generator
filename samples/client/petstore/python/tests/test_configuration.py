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
        pass

    def testConfiguration(self):
        # check that different instances use different dictionaries
        c1 = petstore_api.Configuration()
        c2 = petstore_api.Configuration()
        assert id(c1.api_key) != id(c2.api_key)
        assert id(c1.api_key_prefix) != id(c2.api_key_prefix)


if __name__ == '__main__':
    unittest.main()
