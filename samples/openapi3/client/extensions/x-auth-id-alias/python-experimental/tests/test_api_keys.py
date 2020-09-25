# coding: utf-8

"""
    Test usage of x-auth-id-alias.
"""

import unittest

import x_auth_id_alias
from x_auth_id_alias.api.usage_api import UsageApi


class TestUsageApi(unittest.TestCase):
    """UsageApi unit test stubs"""

    def setUp(self):
        self.configuration = x_auth_id_alias.Configuration(
            host="http://localhost/",
            api_key={"api_key": "SECRET_VALUE"},
            api_key_prefix={"api_key": "PREFIX"},
        )
        self.client = x_auth_id_alias.ApiClient(self.configuration)
        self.api = UsageApi(self.client)

    def tearDown(self):
        self.client.close()

    def test_any_key(self):
        """Test case for any_key

        Use any API key  # noqa: E501
        """

        def request(*args, **kwargs):
            assert ("api_key", "SECRET_VALUE") in kwargs["query_params"]
            assert "PREFIX SECRET_VALUE" == kwargs["headers"]["X-Api-Key"]
            raise RuntimeError("passed")

        self.client.request = request

        try:
            self.api.any_key()
        except RuntimeError as e:
            assert "passed" == str(e)

    def test_both_keys(self):
        """Test case for both_keys

        Use both API keys  # noqa: E501
        """

        def request(*args, **kwargs):
            assert ("api_key", "SECRET_VALUE") in kwargs["query_params"]
            assert "PREFIX SECRET_VALUE" == kwargs["headers"]["X-Api-Key"]
            raise RuntimeError("passed")

        self.client.request = request

        try:
            self.api.both_keys()
        except RuntimeError as e:
            assert "passed" == str(e)

    def test_key_in_header(self):
        """Test case for key_in_header

        Use API key in header  # noqa: E501
        """

        def request(*args, **kwargs):
            assert ("api_key", "SECRET_VALUE") not in kwargs["query_params"]
            assert "PREFIX SECRET_VALUE" == kwargs["headers"]["X-Api-Key"]
            raise RuntimeError("passed")

        self.client.request = request

        try:
            self.api.key_in_header()
        except RuntimeError as e:
            assert "passed" == str(e)

    def test_key_in_query(self):
        """Test case for key_in_query

        Use API key in query  # noqa: E501
        """

        def request(*args, **kwargs):
            assert ("api_key", "SECRET_VALUE") in kwargs["query_params"]
            assert "X-Api-Key" not in kwargs["headers"]
            raise RuntimeError("passed")

        self.client.request = request

        try:
            self.api.key_in_query()
        except RuntimeError as e:
            assert "passed" == str(e)


if __name__ == "__main__":
    unittest.main()
