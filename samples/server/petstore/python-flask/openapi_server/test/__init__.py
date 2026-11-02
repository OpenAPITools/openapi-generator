import logging

import connexion
from flask_testing import TestCase

from openapi_server.encoder import JSONProvider


class BaseTestCase(TestCase):

    def create_app(self):
        logging.getLogger('connexion.operation').setLevel('ERROR')
        app = connexion.App(__name__, specification_dir='../openapi/')
        app.app.json = JSONProvider(app.app)
        app.add_api('openapi.yaml', pythonic_params=True)
        return app.app
