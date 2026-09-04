import logging
import unittest

import connexion
from connexion.jsonifier import Jsonifier

from openapi_server.encoder import JSONEncoder


class _FlaskStyleResponse:
    """Adapts Connexion 3's httpx/Starlette test response to the
    status_code/data attributes the generated controller tests expect."""

    def __init__(self, response):
        self._response = response
        self.status_code = response.status_code
        self.data = response.content


class _FlaskStyleTestClient:
    """Adapts Connexion 3's `app.test_client()` (httpx/Starlette-based) to
    the Flask-style `.open(path, method=, headers=, data=, content_type=,
    query_string=)` calling convention the generated controller tests use."""

    def __init__(self, client):
        self._client = client

    def open(self, path, method='GET', headers=None, data=None,
              content_type=None, query_string=None):
        headers = dict(headers or {})
        if content_type:
            headers['Content-Type'] = content_type
        response = self._client.request(
            method, path, headers=headers, content=data, params=query_string)
        return _FlaskStyleResponse(response)


class BaseTestCase(unittest.TestCase):

    def setUp(self):
        logging.getLogger('connexion.operation').setLevel('ERROR')
        app = connexion.FlaskApp(__name__, specification_dir='../openapi/')
        app.add_api('openapi.yaml', pythonic_params=True,
                     jsonifier=Jsonifier(cls=JSONEncoder))
        self._raw_client_cm = app.test_client()
        self.client = _FlaskStyleTestClient(self._raw_client_cm.__enter__())

    def tearDown(self):
        self._raw_client_cm.__exit__(None, None, None)

    def assert200(self, response, message=None):
        self.assertEqual(200, response.status_code, message)
