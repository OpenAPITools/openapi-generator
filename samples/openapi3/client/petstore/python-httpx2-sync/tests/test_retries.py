"""Configuration.retries is passed to httpx2's transport retries."""
import socket
import unittest
from unittest.mock import patch

import anyio
import httpx2
from petstore_api import Configuration
from petstore_api.rest import RESTClientObject


def closed_port():
    with socket.socket() as sock:
        sock.bind(("127.0.0.1", 0))
        return sock.getsockname()[1]


class TestRetries(unittest.IsolatedAsyncioTestCase):
    async def connection_attempts(self, config):
        attempts = []
        connect_tcp = anyio.connect_tcp

        async def counting_connect_tcp(*args, **kwargs):
            attempts.append(args)
            return await connect_tcp(*args, **kwargs)

        rest = RESTClientObject(config)
        try:
            with patch("anyio.connect_tcp", counting_connect_tcp):
                with self.assertRaises(httpx2.ConnectError):
                    await rest.request("GET", "http://127.0.0.1:%d/pet" % closed_port())
        finally:
            await rest.close()
        return len(attempts)

    async def test_connection_failures_are_retried(self):
        self.assertEqual(await self.connection_attempts(Configuration(retries=2)), 3)

    async def test_no_retry_by_default(self):
        for retries in (None, 0, -1):
            with self.subTest(retries=retries):
                self.assertEqual(await self.connection_attempts(Configuration(retries=retries)), 1)

    async def test_without_retries_the_client_is_built_as_before(self):
        # No custom transport: proxies from environment variables stay honored.
        for retries in (None, 0, -1):
            with self.subTest(retries=retries):
                rest = RESTClientObject(Configuration(retries=retries))
                with patch("petstore_api.rest.httpx2.AsyncHTTPTransport", wraps=httpx2.AsyncHTTPTransport) as transport:
                    pool = rest._create_pool_manager()
                transport.assert_not_called()
                await pool.aclose()

    async def test_proxy_and_tls_go_to_the_transport(self):
        config = Configuration(retries=3)
        config.proxy = "http://localhost:8080"
        config.proxy_headers = {"X-Proxy": "value"}
        rest = RESTClientObject(config)
        with patch("petstore_api.rest.httpx2.AsyncHTTPTransport", wraps=httpx2.AsyncHTTPTransport) as transport, \
                patch("petstore_api.rest.httpx2.AsyncClient", wraps=httpx2.AsyncClient) as factory:
            pool = rest._create_pool_manager()
        kwargs = transport.call_args.kwargs
        self.assertEqual(kwargs["retries"], 3)
        self.assertIs(kwargs["verify"], rest.ssl_context)
        self.assertEqual(kwargs["limits"].max_connections, config.connection_pool_maxsize)
        self.assertEqual(kwargs["proxy"].headers["X-Proxy"], "value")
        # A client-level proxy would mount its own transport and bypass this one.
        self.assertNotIn("proxy", factory.call_args.kwargs)
        await pool.aclose()
