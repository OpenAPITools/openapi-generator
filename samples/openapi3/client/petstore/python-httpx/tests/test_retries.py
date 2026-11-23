"""Configuration.retries is applied to the httpx transport."""
import unittest
from unittest.mock import patch

import httpx
from petstore_api import Configuration
from petstore_api.rest import RESTClientObject


class TestRetries(unittest.IsolatedAsyncioTestCase):
    async def test_without_retries_the_client_builds_its_own_transport(self):
        # No custom transport: proxies from environment variables stay honored.
        rest = RESTClientObject(Configuration())
        with patch("petstore_api.rest.httpx.AsyncHTTPTransport", wraps=httpx.AsyncHTTPTransport) as transport, \
                patch("petstore_api.rest.httpx.AsyncClient", wraps=httpx.AsyncClient) as factory:
            pool = rest._create_pool_manager()
        transport.assert_not_called()
        self.assertNotIn("transport", factory.call_args.kwargs)
        await pool.aclose()

    async def test_retries_proxy_and_tls_go_to_the_transport(self):
        config = Configuration(retries=3)
        config.proxy = "http://localhost:8080"
        config.proxy_headers = {"X-Proxy": "value"}
        rest = RESTClientObject(config)
        with patch("petstore_api.rest.httpx.AsyncHTTPTransport", wraps=httpx.AsyncHTTPTransport) as transport, \
                patch("petstore_api.rest.httpx.AsyncClient", wraps=httpx.AsyncClient) as factory:
            pool = rest._create_pool_manager()
        kwargs = transport.call_args.kwargs
        self.assertEqual(kwargs["retries"], 3)
        self.assertIs(kwargs["verify"], rest.ssl_context)
        self.assertEqual(kwargs["limits"].max_connections, config.connection_pool_maxsize)
        self.assertEqual(kwargs["proxy"].headers["X-Proxy"], "value")
        # A client-level proxy would mount its own transport and bypass the retries.
        self.assertNotIn("proxy", factory.call_args.kwargs)
        self.assertIsInstance(factory.call_args.kwargs["transport"], httpx.AsyncHTTPTransport)
        await pool.aclose()
