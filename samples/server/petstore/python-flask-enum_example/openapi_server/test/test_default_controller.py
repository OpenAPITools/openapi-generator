# coding: utf-8

from __future__ import absolute_import
import unittest

from flask import json
from six import BytesIO

from openapi_server.models.enum_cornucopia import EnumCornucopia  # noqa: E501
from openapi_server.models.outer_enum_integer import OuterEnumInteger  # noqa: E501
from openapi_server.test import BaseTestCase


class TestDefaultController(BaseTestCase):
    """DefaultController integration test stubs"""

    @unittest.skip("application/x-www-form-urlencoded not supported by Connexion")
    def test_enum_get(self):
        """Test case for enum_get

        To test enum parameters
        """
        query_string = [('enum_query_string_array', ['enum_query_string_array_example']),
                        ('enum_query_string', '-efg'),
                        ('enum_query_integer', 56),
                        ('enum_query_double', 1.1)]
        headers = { 
            'Content-Type': 'application/x-www-form-urlencoded',
            'enum_header_string_array': ['enum_header_string_array_example'],
            'enum_header_string': '-efg',
        }
        data = dict(enum_form_string_array='$',
                    enum_form_string='-efg')
        response = self.client.open(
            '/enum-get',
            method='GET',
            headers=headers,
            data=data,
            content_type='application/x-www-form-urlencoded',
            query_string=query_string)
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    @unittest.skip("application/x-www-form-urlencoded not supported by Connexion")
    def test_enum_post_form(self):
        """Test case for enum_post_form

        
        """
        query_string = [('enum_query_int', openapi_server.OuterEnumInteger())]
        headers = { 
            'Accept': '*/*',
            'Content-Type': 'application/x-www-form-urlencoded',
        }
        data = dict(enum_form_string_array='$',
                    enum_form_string='-efg')
        response = self.client.open(
            '/enum-post-form',
            method='POST',
            headers=headers,
            data=data,
            content_type='application/x-www-form-urlencoded',
            query_string=query_string)
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_enum_post_json(self):
        """Test case for enum_post_json

        
        """
        body = {
  "enum_string_required" : "UPPER",
  "enum_integer" : 1,
  "outerEnumInteger" : 2,
  "enum_string" : "UPPER",
  "enum_number" : -1.2
}
        query_string = [('enum_query_int', 2)]
        headers = { 
            'Accept': '*/*',
            'Content-Type': 'application/json',
        }
        response = self.client.open(
            '/enum-post-json',
            method='POST',
            headers=headers,
            data=json.dumps(body),
            content_type='application/json',
            query_string=query_string)
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))


if __name__ == '__main__':
    unittest.main()
