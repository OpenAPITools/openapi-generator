# coding: utf-8

from __future__ import absolute_import
import unittest

from flask import json
from six import BytesIO

from openapi_server.models.enum_cornucopia import EnumCornucopia  # noqa: E501
from openapi_server.test import BaseTestCase


class TestFakeController(BaseTestCase):
    """FakeController integration test stubs"""

    def test_enum_object(self):
        """Test case for enum_object

        
        """
        enum_cornucopia = {
  "enum_string_required" : "UPPER",
  "enum_integer" : 1,
  "outerEnumInteger" : 2,
  "enum_string" : "UPPER",
  "decimal" : "123.455",
  "enum_number" : -1.2
}
        headers = { 
            'Accept': '*/*',
            'Content-Type': 'application/json',
        }
        response = self.client.open(
            '/enum-object',
            method='POST',
            headers=headers,
            data=json.dumps(enum_cornucopia),
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    @unittest.skip("application/x-www-form-urlencoded not supported by Connexion")
    def test_enums(self):
        """Test case for enums

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
            '/enums',
            method='GET',
            headers=headers,
            data=data,
            content_type='application/x-www-form-urlencoded',
            query_string=query_string)
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))


if __name__ == '__main__':
    unittest.main()
