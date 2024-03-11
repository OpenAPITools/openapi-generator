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

import unittest
from unittest.mock import patch, Mock
import petstore_api

class TestStrLikeJson(unittest.TestCase):
    """StrLikeJson unit test stubs"""

    def setUp(self):
        self.api_client = petstore_api.ApiClient()
        self.fake_api = petstore_api.FakeApi(self.api_client)


    def testStrLikeJson(self):
        """Test StrLikeJson"""
        mock_resp = Mock()
        mock_resp.status = 200
        mock_resp.data = b'{"a": "a"}'
        mock_resp.getheaders.return_value = {}
        mock_resp.getheader = (
            lambda name: "text/plain" if name == "content-type" else Mock()
        )

        
        with patch(
            "petstore_api.api_client.ApiClient.call_api", return_value=mock_resp
        ):
            returned = self.fake_api.fake_str_like_json()
            self.assertEqual('{"a": "a"}', returned)
            print(returned)