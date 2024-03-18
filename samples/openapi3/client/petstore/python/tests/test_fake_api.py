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

class TestFakeApi(unittest.TestCase):
    """StrLikeJson unit test stubs"""

    def setUp(self):
        self.api_client = petstore_api.ApiClient()
        self.fake_api = petstore_api.FakeApi(self.api_client)

    def testReturnString(self):
        """Test ReturnString"""
        mock_resp = Mock()
        mock_resp.status = 200
        mock_resp.data = b'string'
        mock_resp.getheaders.return_value = {}
        mock_resp.getheader = (
            lambda name: "text/plain" if name == "content-type" else Mock()
        )
        with patch(
            "petstore_api.api_client.ApiClient.call_api", return_value=mock_resp
        ):
            returned = self.fake_api.fake_return_string()
            self.assertEqual("string", returned)
    
    def testReturnInt(self):
        """Test ReturnInt"""
        mock_resp = Mock()
        mock_resp.status = 200
        mock_resp.data = b'1'
        mock_resp.getheaders.return_value = {}
        mock_resp.getheader = (
            lambda name: "text/plain" if name == "content-type" else Mock()
        )
        with patch(
            "petstore_api.api_client.ApiClient.call_api", return_value=mock_resp
        ):
            returned = self.fake_api.fake_return_int()
            self.assertEqual(1, returned)
    
    def testReturnFloat(self):
        """Test ReturnFloat"""
        mock_resp = Mock()
        mock_resp.status = 200
        mock_resp.data = b'3.4'
        mock_resp.getheaders.return_value = {}
        mock_resp.getheader = (
            lambda name: "text/plain" if name == "content-type" else Mock()
        )
        with patch(
            "petstore_api.api_client.ApiClient.call_api", return_value=mock_resp
        ):
            returned = self.fake_api.fake_return_float()
            self.assertEqual(3.4, returned)
    
    def testReturnBoolean(self):
        """Test ReturnBool"""
        mock_resp = Mock()
        mock_resp.status = 200
        mock_resp.data = b'true'
        mock_resp.getheaders.return_value = {}
        mock_resp.getheader = (
            lambda name: "text/plain" if name == "content-type" else Mock()
        )
        with patch(
            "petstore_api.api_client.ApiClient.call_api", return_value=mock_resp
        ):
            returned = self.fake_api.fake_return_boolean()
            self.assertEqual(True, returned)
    
    def testReturnEnum(self):
        """Test ReturnEnum"""
        mock_resp = Mock()
        mock_resp.status = 200
        mock_resp.data = b'a'
        mock_resp.getheaders.return_value = {}
        mock_resp.getheader = (
            lambda name: "text/plain" if name == "content-type" else Mock()
        )
        with patch(
            "petstore_api.api_client.ApiClient.call_api", return_value=mock_resp
        ):
            returned = self.fake_api.fake_return_enum()
            self.assertEqual("a", returned)

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
            returned = self.fake_api.fake_return_str_like_json()
            self.assertEqual('{"a": "a"}', returned)
        
    def testEnumLikeJson(self):
        """Test EnumLikeJson"""
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
            returned = self.fake_api.fake_return_enum_like_json()
            self.assertEqual('{"a": "a"}', returned)
    
    def testByteLikeJson(self):
        """Test ByteLikeJson"""
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
            returned = self.fake_api.fake_return_byte_like_json()
            self.assertEqual(b'{"a": "a"}', returned)