"""Regression tests for request-local, case-insensitive HTTPX headers."""

import asyncio
from concurrent.futures import ThreadPoolExecutor
import importlib
import unittest
from typing import Any

import petstore_api
from petstore_api import rest

httpx = importlib.import_module(rest.RESTResponseType.__module__.split('.')[0])


class TestRequestHeaders(unittest.IsolatedAsyncioTestCase):
    async def asyncSetUp(self):
        self.requests: list[Any] = []

        async def respond(request):
            self.requests.append(request)
            await asyncio.sleep(0)
            return httpx.Response(200, json={})

        self.client = petstore_api.ApiClient(petstore_api.Configuration(
            host='https://example.test', access_token='configured-token',
            api_key={'api_key': 'configured-key'},
        ))
        self.client.rest_client.pool_manager = httpx.AsyncClient(
            transport=httpx.MockTransport(respond)
        )
        self.api = petstore_api.PetApi(self.client)
        self.pet = petstore_api.Pet(name='test', photoUrls=[])

    async def asyncTearDown(self):
        await self.client.close()

    def assert_single_header(self, request, name, value):
        self.assertEqual(request.headers.get_list(name), [value])

    async def test_serializers_copy_header_inputs_before_adding_defaults_and_auth(self):
        headers = {'content-type': 'application/json', 'authorization': 'caller'}
        original = headers.copy()
        defaults = self.client.default_headers.copy()
        self.api._add_pet_serialize(
            pet=self.pet, _headers=headers, _request_auth=None,
            _content_type=None, _host_index=0,
        )
        self.assertEqual(headers, original)
        self.client.param_serialize(
            'GET', '/pet', header_params=headers, query_params=[], auth_settings=['petstore_auth']
        )
        self.assertEqual(headers, original)
        self.assertEqual(self.client.default_headers, defaults)

    async def test_generated_content_type_replaces_case_variants(self):
        for spelling in ('Content-Type', 'content-type', 'CONTENT-TYPE'):
            for value in ('application/json', 'text/plain'):
                with self.subTest(spelling=spelling, value=value):
                    headers = {spelling: value, 'accept': 'application/custom'}
                    original = headers.copy()
                    await self.api.add_pet(self.pet, _headers=headers)
                    self.assert_single_header(
                        self.requests[-1], 'content-type', 'application/json'
                    )
                    self.assert_single_header(self.requests[-1], 'accept', 'application/custom')
                    self.assertEqual(headers, original)

    async def test_content_type_override_and_client_default_precedence(self):
        headers = {'content-type': 'text/plain'}
        await self.api.add_pet(self.pet, _headers=headers, _content_type='application/custom+json')
        self.assert_single_header(self.requests[-1], 'content-type', 'application/custom+json')
        self.client.set_default_header('CONTENT-TYPE', 'application/default+json')
        defaults = self.client.default_headers.copy()
        await self.api.add_pet(self.pet, _headers=headers, _content_type='application/custom+json')
        self.assert_single_header(self.requests[-1], 'content-type', 'application/default+json')
        self.assertEqual(self.client.default_headers, defaults)
        self.assertEqual(headers, {'content-type': 'text/plain'})

    async def test_defaults_and_last_inserted_case_variant_win_without_splitting_values(self):
        self.client.set_default_header('X-Default', 'earlier-default')
        self.client.set_default_header('x-default', 'default')
        headers = {'x-default': 'call', 'X-List': 'first', 'x-list': 'a, b, a'}
        await self.api.add_pet(self.pet, _headers=headers)
        self.assert_single_header(self.requests[-1], 'x-default', 'default')
        self.assert_single_header(self.requests[-1], 'x-list', 'a, b, a')
        self.assertIn('x-list', self.requests[-1].headers)
        self.assertEqual(headers['X-List'], 'first')

    async def test_authentication_overrides_case_variants_and_preserves_inputs(self):
        self.client.set_default_header('AUTHORIZATION', 'default')
        defaults = self.client.default_headers.copy()
        headers = {'authorization': 'call'}
        await self.api.add_pet(self.pet, _headers=headers)
        self.assert_single_header(self.requests[-1], 'authorization', 'Bearer configured-token')
        auth = {'in': 'header', 'key': 'Authorization', 'type': 'oauth2', 'value': 'Bearer call'}
        await self.api.add_pet(self.pet, _headers=headers, _request_auth=auth)
        self.assert_single_header(self.requests[-1], 'authorization', 'Bearer call')
        self.assertEqual(headers, {'authorization': 'call'})
        self.assertEqual(auth['value'], 'Bearer call')
        self.assertEqual(self.client.default_headers, defaults)

    async def test_api_key_overrides_default_and_call_case_variants(self):
        self.client.set_default_header('API_KEY', 'default')
        headers = {'Api_Key': 'call'}
        defaults = self.client.default_headers.copy()
        params = self.client.param_serialize(
            'GET', '/key', header_params=headers, auth_settings=['api_key'], query_params=[]
        )
        await self.client.call_api(*params)
        self.assert_single_header(self.requests[-1], 'api_key', 'configured-key')
        self.assertEqual(headers, {'Api_Key': 'call'})
        self.assertEqual(self.client.default_headers, defaults)

    async def test_cookie_authentication_appends_to_selected_cookie(self):
        self.client.cookie = 'client=1'
        headers = {'cookie': 'call=1'}
        params = self.client.param_serialize(
            'GET', '/cookie', header_params=headers, query_params=[], auth_settings=['cookie'],
            _request_auth={'in': 'cookie', 'key': 'session', 'type': 'api_key', 'value': 'two'},
        )
        await self.client.call_api(*params)
        self.assert_single_header(self.requests[-1], 'cookie', 'client=1; session=two')
        self.assertEqual(headers, {'cookie': 'call=1'})

    async def test_concurrent_calls_do_not_share_headers_or_authentication(self):
        headers = {'content-type': 'application/json', 'authorization': 'caller'}
        defaults = self.client.default_headers.copy()

        async def call(index):
            await self.api.add_pet(self.pet, _headers=headers, _request_auth={
                'in': 'header', 'key': 'Authorization', 'type': 'oauth2',
                'value': f'Bearer request-{index}',
            })

        await asyncio.gather(*(call(index) for index in range(20)))
        self.assertCountEqual(
            [request.headers['authorization'] for request in self.requests],
            [f'Bearer request-{index}' for index in range(20)],
        )
        for request in self.requests:
            self.assert_single_header(request, 'content-type', 'application/json')
        await self.api.add_pet(self.pet)
        self.assert_single_header(self.requests[-1], 'authorization', 'Bearer configured-token')
        self.assertEqual(headers, {'content-type': 'application/json', 'authorization': 'caller'})
        self.assertEqual(self.client.default_headers, defaults)

    async def test_public_mapping_validation_retains_combined_values(self):
        headers = httpx.Headers([('X-List', 'a'), ('x-list', 'b')])
        await self.api.add_pet(self.pet, _headers=headers)
        self.assert_single_header(self.requests[-1], 'x-list', 'a, b')
        self.assertEqual(headers.get_list('x-list'), ['a', 'b'])

    async def test_transport_reads_lowercase_content_type_and_preserves_repeated_fields(self):
        headers = httpx.Headers([
            ('content-type', 'application/x-www-form-urlencoded'),
            ('x-list', 'a'), ('x-list', 'b'),
        ])
        await self.client.rest_client.request(
            'POST', 'https://example.test/form', headers=headers, post_params=[('field', 'value')]
        )
        self.assert_single_header(
            self.requests[-1], 'content-type', 'application/x-www-form-urlencoded'
        )
        self.assertEqual(self.requests[-1].content, b'field=value')
        self.assertEqual(self.requests[-1].headers.get_list('x-list'), ['a', 'b'])
        self.assertEqual(headers.get_list('x-list'), ['a', 'b'])

    async def test_transport_multipart_does_not_mutate_input(self):
        headers = {'content-type': 'multipart/form-data'}
        await self.client.rest_client.request(
            'POST', 'https://example.test/upload', headers=headers,
            post_params=[('file', ('name.txt', b'content', 'text/plain'))],
        )
        self.assertTrue(
            self.requests[-1].headers['content-type'].startswith('multipart/form-data;')
        )
        self.assertEqual(len(self.requests[-1].headers.get_list('content-type')), 1)
        self.assertEqual(headers, {'content-type': 'multipart/form-data'})

    async def test_sync_calls_use_request_local_headers(self):
        if not hasattr(self.api, 'add_pet_sync'):
            self.skipTest('Sample does not enable supportHttpxSync')
        headers = {'content-type': 'application/json', 'authorization': 'caller'}
        defaults = self.client.default_headers.copy()

        def call(index):
            getattr(self.api, 'add_pet_sync')(self.pet, _headers=headers, _request_auth={
                'in': 'header', 'key': 'Authorization', 'type': 'oauth2',
                'value': f'Bearer sync-{index}',
            })

        def run_calls():
            with ThreadPoolExecutor(max_workers=4) as executor:
                list(executor.map(call, range(8)))

        await asyncio.to_thread(run_calls)
        self.assertCountEqual(
            [request.headers['authorization'] for request in self.requests],
            [f'Bearer sync-{index}' for index in range(8)],
        )
        for request in self.requests:
            self.assert_single_header(request, 'content-type', 'application/json')
        self.assertEqual(headers, {'content-type': 'application/json', 'authorization': 'caller'})
        self.assertEqual(self.client.default_headers, defaults)
