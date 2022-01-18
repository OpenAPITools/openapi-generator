# coding: utf-8

from __future__ import absolute_import
import unittest

from flask import json
from six import BytesIO

from openapi_server.models.client import Client  # noqa: E501
from openapi_server.test import BaseTestCase


class TestAnotherFakeController(BaseTestCase):
    """AnotherFakeController integration test stubs"""

    def test_call123_test_special_tags(self):
        """Test case for call123_test_special_tags

        To test special tags
        """
        client = {
  "client" : "client"
}
        headers = { 
            'Accept': 'application/json',
            'Content-Type': 'application/json',
        }
        response = self.client.open(
            '/v2/another-fake/dummy',
            method='PATCH',
            headers=headers,
            data=json.dumps(client),
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))


if __name__ == '__main__':
    unittest.main()
