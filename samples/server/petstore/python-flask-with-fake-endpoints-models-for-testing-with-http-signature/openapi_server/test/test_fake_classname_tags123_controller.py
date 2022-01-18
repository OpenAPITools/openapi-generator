# coding: utf-8

from __future__ import absolute_import
import unittest

from flask import json
from six import BytesIO

from openapi_server.models.client import Client  # noqa: E501
from openapi_server.test import BaseTestCase


class TestFakeClassnameTags123Controller(BaseTestCase):
    """FakeClassnameTags123Controller integration test stubs"""

    def test_test_classname(self):
        """Test case for test_classname

        To test class name in snake case
        """
        client = {
  "client" : "client"
}
        headers = { 
            'Accept': 'application/json',
            'Content-Type': 'application/json',
            'api_key_query': 'special-key',
        }
        response = self.client.open(
            '/v2/fake_classname_test',
            method='PATCH',
            headers=headers,
            data=json.dumps(client),
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))


if __name__ == '__main__':
    unittest.main()
