import logging
from unittest.mock import patch

import connexion
from flask_testing import TestCase

from openapi_server.encoder import JSONEncoder


class BaseTestCase(TestCase):
    def create_app(self):
        logging.getLogger("connexion.operation").setLevel("ERROR")
        app = connexion.App(__name__, specification_dir="../openapi/")
        app.app.json_encoder = JSONEncoder
        app.add_api("openapi.yaml", pythonic_params=True)
        return app.app

    def setUp(self):
        impl_patcher = patch("openapi_server.impl", create=True)
        impl_patcher.start()
        self.addCleanup(impl_patcher)
