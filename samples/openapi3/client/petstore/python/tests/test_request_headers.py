"""Request-header behavior for the urllib3 client."""

import asyncio
from concurrent.futures import ThreadPoolExecutor
import inspect
import unittest
from typing import Any
from unittest.mock import AsyncMock, Mock

import petstore_api

ASYNC = inspect.iscoroutinefunction(petstore_api.ApiClient.call_api)


class TestRequestHeaders(unittest.IsolatedAsyncioTestCase):
    async def asyncSetUp(self):
        self.requests: list[dict[str, Any]] = []
        self.client = petstore_api.ApiClient(petstore_api.Configuration(
            host='https://example.test', access_token='configured-token',
            api_key={'api_key': 'configured-key'},
        ))
        response = Mock(status=200, reason='OK', data=b'{}', headers={})
        response.read = AsyncMock(return_value=b'{}')

        def respond(*args, **kwargs):
            self.requests.append({**kwargs, 'headers': kwargs['headers'].copy()})
            return response

        async def respond_async(*args, **kwargs):
            await asyncio.sleep(0)
            return respond(*args, **kwargs)

        self.pool: Any = AsyncMock() if ASYNC else Mock()
        self.pool.request.side_effect = respond_async if ASYNC else respond
        self.client.rest_client.pool_manager = self.pool
        self.api = petstore_api.PetApi(self.client)
        self.pet = petstore_api.Pet(name='test', photoUrls=[])

    async def asyncTearDown(self):
        if ASYNC:
            await getattr(self.client, 'close')()
        else:
            self.pool.clear()

    async def call(self, **kwargs):
        result = self.api.add_pet(self.pet, **kwargs)
        if inspect.isawaitable(result):
            await result

    def values(self, headers, name):
        if isinstance(headers, dict):
            return [value for key, value in headers.items() if key.lower() == name.lower()]
        return headers.getall(name) if ASYNC else headers.getlist(name)

    def assert_header(self, name, value):
        self.assertEqual(self.values(self.requests[-1]['headers'], name), [value])

    async def test_generated_content_type_and_accept_are_case_insensitive(self):
        for spelling in ('Content-Type', 'content-type', 'CONTENT-TYPE'):
            for value in ('application/json', 'text/plain'):
                with self.subTest(spelling=spelling, value=value):
                    headers = {spelling: value, 'accept': 'custom'}
                    await self.call(_headers=headers)
                    self.assert_header('content-type', 'application/json')
                    self.assert_header('accept', 'custom')
                    self.assertEqual(headers, {spelling: value, 'accept': 'custom'})

    async def test_user_agent_matches_last_direct_default_header_write(self):
        self.client.default_headers['user-agent'] = 'custom'
        self.client.default_headers['USER-AGENT'] = 'latest'
        defaults = self.client.default_headers.copy()
        self.assertEqual(self.client.user_agent, 'latest')
        await self.call()
        self.assert_header('user-agent', 'latest')
        self.assertEqual(self.client.default_headers, defaults)

    async def test_content_type_override_and_defaults_retain_precedence(self):
        headers = {'content-type': 'text/plain'}
        await self.call(_headers=headers, _content_type='application/custom+json')
        self.assert_header('content-type', 'application/custom+json')
        self.client.set_default_header('CONTENT-TYPE', 'application/default+json')
        defaults = self.client.default_headers.copy()
        await self.call(_headers=headers, _content_type='application/custom+json')
        self.assert_header('content-type', 'application/default+json')
        self.assertEqual(self.client.default_headers, defaults)
        self.assertEqual(headers, {'content-type': 'text/plain'})

    async def test_defaults_last_write_and_user_agent_property(self):
        self.client.set_default_header('X-Default', 'old')
        self.client.set_default_header('x-default', 'middle')
        self.client.set_default_header('X-Default', 'new')
        self.client.set_default_header('user-agent', 'custom')
        self.assertEqual(self.client.user_agent, 'custom')
        headers = {'x-default': 'call', 'X-List': 'first', 'x-list': 'a, b, a'}
        await self.call(_headers=headers)
        self.assert_header('x-default', 'new')
        self.assert_header('user-agent', 'custom')
        self.assert_header('x-list', 'a, b, a')
        self.assertEqual(headers['X-List'], 'first')
        self.client.user_agent = 'reset'
        self.assertEqual(self.client.user_agent, 'reset')
        self.assertEqual(
            len([key for key in self.client.default_headers if key.lower() == 'user-agent']), 1
        )

    async def test_serializers_preserve_inputs_and_explicit_header_wins(self):
        headers = {'content-type': 'text/plain', 'API_KEY': 'call'}
        original = headers.copy()
        defaults = self.client.default_headers.copy()
        params = self.api._delete_pet_serialize(
            pet_id=1, api_key='explicit', _headers=headers, _request_auth=None,
            _content_type=None, _host_index=0,
        )
        self.assertEqual(params[2]['api_key'], 'explicit')
        self.api._add_pet_serialize(
            pet=self.pet, _headers=headers, _request_auth=None,
            _content_type=None, _host_index=0,
        )
        self.client.param_serialize('GET', '/pet', header_params=headers, query_params=[])
        self.assertEqual(headers, original)
        self.assertEqual(self.client.default_headers, defaults)

    async def test_authentication_and_empty_override(self):
        self.client.set_default_header('AUTHORIZATION', 'default')
        headers = {'authorization': 'call'}
        defaults = self.client.default_headers.copy()
        await self.call(_headers=headers, _request_auth={})
        self.assert_header('authorization', 'Bearer configured-token')
        auth = {'in': 'header', 'key': 'Authorization', 'type': 'oauth2', 'value': 'Bearer call'}
        await self.call(_headers=headers, _request_auth=auth)
        self.assert_header('authorization', 'Bearer call')
        self.assertEqual(headers, {'authorization': 'call'})
        self.assertEqual(auth['value'], 'Bearer call')
        self.assertEqual(self.client.default_headers, defaults)
        self.client.set_default_header('API_KEY', 'default')
        key_headers = {'Api_Key': 'call'}
        params = self.client.param_serialize(
            'GET', '/key', header_params=key_headers, auth_settings=['api_key'], query_params=[]
        )
        result = self.client.call_api(*params)
        if inspect.isawaitable(result):
            await result
        self.assert_header('api_key', 'configured-key')
        self.assertEqual(key_headers, {'Api_Key': 'call'})

    async def test_cookie_authentication_appends_without_empty_separator(self):
        for value, expected in (('', 'session=two'), ('client=1', 'client=1; session=two')):
            headers = {'cookie': value}
            params = self.client.param_serialize(
                'GET', '/cookie', header_params=headers, query_params=[],
                auth_settings=['cookie'],
                _request_auth={
                    'in': 'cookie', 'key': 'session', 'type': 'api_key', 'value': 'two',
                },
            )
            result = self.client.call_api(*params)
            if inspect.isawaitable(result):
                await result
            self.assert_header('cookie', expected)
            self.assertEqual(headers, {'cookie': value})

    async def test_concurrent_calls_keep_inputs_defaults_and_credentials_isolated(self):
        headers = {'content-type': 'application/json', 'authorization': 'caller'}
        defaults = self.client.default_headers.copy()

        def auth(index):
            return {'in': 'header', 'key': 'Authorization', 'type': 'oauth2',
                    'value': f'Bearer request-{index}'}

        if ASYNC:
            await asyncio.gather(*(
                self.call(_headers=headers, _request_auth=auth(index)) for index in range(20)
            ))
        else:
            def run_calls():
                with ThreadPoolExecutor(max_workers=4) as executor:
                    list(executor.map(lambda index: self.api.add_pet(
                        self.pet, _headers=headers, _request_auth=auth(index)
                    ), range(20)))
            await asyncio.to_thread(run_calls)
        self.assertCountEqual(
            [request['headers']['authorization'] for request in self.requests],
            [f'Bearer request-{index}' for index in range(20)],
        )
        await self.call()
        self.assert_header('authorization', 'Bearer configured-token')
        self.assertEqual(headers, {'content-type': 'application/json', 'authorization': 'caller'})
        self.assertEqual(self.client.default_headers, defaults)

    async def test_transport_reads_lowercase_content_type(self):
        headers = {'content-type': 'application/x-www-form-urlencoded'}
        result = self.client.rest_client.request(
            'POST', 'https://example.test/form', headers=headers, post_params=[('field', 'value')]
        )
        if inspect.isawaitable(result):
            await result
        self.assert_header('content-type', 'application/x-www-form-urlencoded')
        self.assertFalse(self.requests[-1].get('encode_multipart', True))
        self.assertEqual(self.requests[-1]['fields'], [('field', 'value')])
        self.assertEqual(headers, {'content-type': 'application/x-www-form-urlencoded'})

    async def test_transport_reads_lowercase_content_type_and_preserves_repeated_fields(self):
        import urllib3
        headers = urllib3.HTTPHeaderDict({'content-type': 'application/x-www-form-urlencoded'})
        headers.add('x-list', 'a')
        headers.add('x-list', 'b')
        result = self.client.rest_client.request(
            'POST', 'https://example.test/form', headers=headers, post_params=[('field', 'value')]
        )
        if inspect.isawaitable(result):
            await result
        self.assert_header('content-type', 'application/x-www-form-urlencoded')
        self.assertEqual(self.values(self.requests[-1]['headers'], 'x-list'), ['a', 'b'])
        self.assertEqual(self.values(headers, 'x-list'), ['a', 'b'])
        self.assertFalse(self.requests[-1]['encode_multipart'])
        self.assertEqual(self.requests[-1]['fields'], [('field', 'value')])

    async def test_transport_multipart_removes_content_type_without_mutating_input(self):
        headers = {'content-type': 'multipart/form-data'}
        result = self.client.rest_client.request(
            'POST', 'https://example.test/upload', headers=headers,
            post_params=[('file', ('name.txt', b'content', 'text/plain'))],
        )
        if inspect.isawaitable(result):
            await result
        self.assertNotIn('content-type', self.requests[-1]['headers'])
        self.assertEqual(headers, {'content-type': 'multipart/form-data'})
