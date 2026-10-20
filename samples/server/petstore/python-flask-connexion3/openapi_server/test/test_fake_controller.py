import unittest

from flask import json

from openapi_server.test import BaseTestCase


class TestFakeController(BaseTestCase):
    """FakeController integration test stubs"""

    def test_fake_query_param_default(self):
        """Test case for fake_query_param_default

        test query parameter default value
        """
        query_string = [('hasDefault', 'Hello World'),
                        ('noDefault', 'no_default_example')]
        headers = { 
        }
        response = self.client.open(
            '/v2/fake/query_param_default',
            method='GET',
            headers=headers,
            query_string=query_string)
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))


if __name__ == '__main__':
    unittest.main()
