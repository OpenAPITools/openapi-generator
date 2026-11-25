"""Configuration.retries is passed to httpx's transport retries, for direct connections."""
import os
import socket
import unittest
from contextlib import contextmanager
from unittest.mock import patch

import anyio
import httpx
from petstore_api import Configuration
from petstore_api.rest import RESTClientObject


def closed_port():
    with socket.socket() as sock:
        sock.bind(("127.0.0.1", 0))
        return sock.getsockname()[1]


@contextmanager
def proxy_environment(**proxies):
    """Replace the *_proxy environment variables with the given ones."""
    environ = {k: v for k, v in os.environ.items() if not k.lower().endswith("_proxy")}
    environ.update(proxies)
    with patch.dict(os.environ, environ, clear=True):
        yield


class TestRetries(unittest.IsolatedAsyncioTestCase):
    async def connection_attempts(self, config):
        attempts = []
        connect_tcp = anyio.connect_tcp

        async def counting_connect_tcp(*args, **kwargs):
            attempts.append(args)
            return await connect_tcp(*args, **kwargs)

        rest = RESTClientObject(config)
        try:
            with proxy_environment(), patch("anyio.connect_tcp", counting_connect_tcp):
                with self.assertRaises(httpx.ConnectError):
                    await rest.request("GET", "http://127.0.0.1:%d/pet" % closed_port())
        finally:
            await rest.close()
        return len(attempts)

    def build_client(self, config):
        """Return the REST client, the transport kwargs (None if no custom transport) and the client kwargs."""
        rest = RESTClientObject(config)
        with patch("petstore_api.rest.httpx.AsyncHTTPTransport", wraps=httpx.AsyncHTTPTransport) as transport, \
                patch("petstore_api.rest.httpx.AsyncClient", wraps=httpx.AsyncClient) as factory:
            pool = rest._create_pool_manager()
        self.addAsyncCleanup(pool.aclose)
        transport_kwargs = transport.call_args.kwargs if transport.called else None
        return rest, transport_kwargs, factory.call_args.kwargs

    async def test_connection_failures_are_retried(self):
        self.assertEqual(await self.connection_attempts(Configuration(retries=2)), 3)

    async def test_no_retry_by_default(self):
        for retries in (None, 0, -1):
            with self.subTest(retries=retries):
                self.assertEqual(await self.connection_attempts(Configuration(retries=retries)), 1)

    async def test_retry_transport_gets_tls_and_limits(self):
        config = Configuration(retries=3)
        with proxy_environment():
            rest, transport_kwargs, client_kwargs = self.build_client(config)
        self.assertEqual(transport_kwargs["retries"], 3)
        self.assertIs(transport_kwargs["verify"], rest.ssl_context)
        self.assertEqual(transport_kwargs["limits"].max_connections, config.connection_pool_maxsize)
        self.assertIsInstance(client_kwargs["transport"], httpx.AsyncHTTPTransport)

    async def test_no_proxy_alone_keeps_retries(self):
        with proxy_environment(NO_PROXY="example.com"):
            _, transport_kwargs, _ = self.build_client(Configuration(retries=3))
        self.assertEqual(transport_kwargs["retries"], 3)

    # With any proxy, httpx does not retry (encode/httpx#2997): the client must be
    # built exactly as without retries, so that proxies keep working as before.

    async def test_without_retries_the_client_is_built_as_before(self):
        for retries in (None, 0, -1):
            with self.subTest(retries=retries), proxy_environment():
                _, transport_kwargs, client_kwargs = self.build_client(Configuration(retries=retries))
                self.assertIsNone(transport_kwargs)
                self.assertNotIn("transport", client_kwargs)

    async def test_explicit_proxy_keeps_the_client_as_before(self):
        config = Configuration(retries=3)
        config.proxy = "http://localhost:8080"
        config.proxy_headers = {"X-Proxy": "value"}
        with proxy_environment():
            _, transport_kwargs, client_kwargs = self.build_client(config)
        self.assertIsNone(transport_kwargs)
        self.assertNotIn("transport", client_kwargs)
        self.assertEqual(client_kwargs["proxy"].headers["X-Proxy"], "value")

    async def test_environment_proxy_keeps_the_client_as_before(self):
        for variable in ("HTTP_PROXY", "https_proxy", "ALL_PROXY"):
            with self.subTest(variable=variable), proxy_environment(**{variable: "http://localhost:8080"}):
                _, transport_kwargs, client_kwargs = self.build_client(Configuration(retries=3))
                self.assertIsNone(transport_kwargs)
                self.assertNotIn("transport", client_kwargs)
