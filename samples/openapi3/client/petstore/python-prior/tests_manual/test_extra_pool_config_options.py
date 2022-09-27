import unittest
from unittest.mock import patch

import petstore_api


class StubPoolManager(object):
    actual_kwargs = None

    def __init__(self, num_pools=10, headers=None, **kwargs):
        # Matches the contract of urllib3.PoolManager
        self.actual_kwargs = kwargs


class StubProxyManager:
    actual_kwargs = None

    def __init__(
        self,
        proxy_url,
        num_pools=10,
        headers=None,
        proxy_headers=None,
        **kwargs
    ):
        # Matches the contract of urllib3.ProxyManager
        self.actual_kwargs = kwargs


class TestExtraOptionsForPools(unittest.TestCase):

    def test_socket_options_get_passed_to_pool_manager(self):

        socket_options = ["extra", "socket", "options"]

        config = petstore_api.Configuration(host="HOST")
        config.socket_options = socket_options

        with patch("petstore_api.rest.urllib3.PoolManager", StubPoolManager):
            api_client = petstore_api.ApiClient(config)

        # urllib3.PoolManager promises to pass socket_options in kwargs
        # to the underlying socket. So asserting that our manager
        # gets it is a good start
        assert api_client.rest_client.pool_manager.actual_kwargs["socket_options"] == socket_options

    def test_socket_options_get_passed_to_proxy_manager(self):

        socket_options = ["extra", "socket", "options"]

        config = petstore_api.Configuration(host="http://somehost.local:8080")
        config.socket_options = socket_options
        config.proxy = "http://proxy.local:8080/"

        with patch("petstore_api.rest.urllib3.ProxyManager", StubProxyManager):
            api_client = petstore_api.ApiClient(config)

        # urllib3.ProxyManager promises to pass socket_options in kwargs
        # to the underlying socket. So asserting that our manager
        # gets it is a good start
        assert api_client.rest_client.pool_manager.actual_kwargs["socket_options"] == socket_options
