"""Exercise the generated transport with real HTTPX2 requests and responses."""
import json
import ssl
import unittest
from unittest.mock import patch

import httpx2
from petstore_api import ApiClient, Configuration, PetApi
from petstore_api.exceptions import NotFoundException
from petstore_api.rest import RESTClientObject


class TestHttpx2Transport(unittest.IsolatedAsyncioTestCase):
    async def test_request_encodings_timeouts_and_responses(self):
        requests = []

        def handle(request):
            requests.append(request)
            return httpx2.Response(200, json={"id": 1, "name": "dog", "photoUrls": []})

        rest = RESTClientObject(Configuration())
        rest.pool_manager = httpx2.AsyncClient(transport=httpx2.MockTransport(handle))
        try:
            response = await rest.request(
                "POST", "https://example.test/pet",
                body={"name": "dog"}, _request_timeout=(2, 3),
            )
            self.assertEqual(json.loads(requests[-1].content), {"name": "dog"})
            self.assertEqual(requests[-1].extensions["timeout"]["connect"], 2)
            self.assertEqual(requests[-1].extensions["timeout"]["read"], 3)
            self.assertEqual(response.status, 200)
            self.assertEqual(json.loads(await response.read())["id"], 1)
            await rest.request(
                "POST", "https://example.test/pet",
                headers={"Content-Type": "application/x-www-form-urlencoded"},
                post_params=[("name", "a b")],
            )
            self.assertEqual(requests[-1].content, b"name=a+b")
            await rest.request(
                "POST", "https://example.test/pet",
                headers={"Content-Type": "multipart/form-data"},
                post_params=[("file", ("hello.txt", b"hello", "text/plain")), ("count", 2)],
            )
            self.assertIn(b"hello", requests[-1].content)
            self.assertIn("boundary=", requests[-1].headers["content-type"])
        finally:
            await rest.close()
        self.assertTrue(rest.pool_manager.is_closed)

    async def test_api_http_error(self):
        async with ApiClient() as client:
            client.rest_client.pool_manager = httpx2.AsyncClient(
                transport=httpx2.MockTransport(
                    lambda request: httpx2.Response(404, json={"message": "missing"}),
                ),
            )
            with self.assertRaises(NotFoundException):
                await PetApi(client).get_pet_by_id(1)

    async def test_transport_timeout(self):
        def handle(request):
            raise httpx2.ReadTimeout("timed out", request=request)

        rest = RESTClientObject(Configuration())
        rest.pool_manager = httpx2.AsyncClient(transport=httpx2.MockTransport(handle))
        try:
            with self.assertRaises(httpx2.ReadTimeout):
                await rest.request("GET", "https://example.test/pet")
        finally:
            await rest.close()

    async def test_proxy_and_tls_configuration(self):
        config = Configuration()
        config.proxy = "http://localhost:8080"
        config.proxy_headers = {"X-Proxy": "value"}
        config.verify_ssl = False
        rest = RESTClientObject(config)
        self.assertEqual(rest.ssl_context.verify_mode, ssl.CERT_NONE)
        self.assertFalse(rest.ssl_context.check_hostname)
        with patch("petstore_api.rest.httpx2.AsyncClient", wraps=httpx2.AsyncClient) as factory:
            pool = rest._create_pool_manager()
            self.assertIs(factory.call_args.kwargs["verify"], rest.ssl_context)
            self.assertEqual(factory.call_args.kwargs["proxy"].headers["X-Proxy"], "value")
            await pool.aclose()
        self.assertEqual(RESTClientObject(Configuration()).ssl_context.verify_mode, ssl.CERT_REQUIRED)
