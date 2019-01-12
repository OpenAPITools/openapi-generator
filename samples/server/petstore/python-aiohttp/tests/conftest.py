import logging
import pytest
import os

import connexion


@pytest.fixture
def client(loop, aiohttp_client):
    logging.getLogger('connexion.operation').setLevel('ERROR')
    options = {
        "swagger_ui": True
        }
    specification_dir = os.path.join(os.path.dirname(__file__), '..', 'openapi_server', 'openapi')
    app = connexion.AioHttpApp(__name__, specification_dir=specification_dir, options=options)
    app.add_api('openapi.yaml', pass_context_arg_name='request')
    return loop.run_until_complete(aiohttp_client(app.app))
