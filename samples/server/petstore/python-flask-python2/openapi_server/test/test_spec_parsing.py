# coding: utf-8

import connexion
import logging
from openapi_server.encoder import JSONEncoder
from unittest import TestCase

class TestSpecParsing(TestCase):
    def test(self):
        with self.assertRaises(TypeError):
            logging.getLogger('connexion.operation').setLevel('ERROR')
            app = connexion.App(__name__, specification_dir='../openapi/')
            app.app.json_encoder = JSONEncoder
            app.add_api('broken_openapi.yaml', pythonic_params=True)
            self.assertTrue(app.app is not None)
