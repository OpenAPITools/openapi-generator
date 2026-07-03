#!/usr/bin/env python3

import connexion
from connexion.jsonifier import Jsonifier

from openapi_server import encoder


def main():
    app = connexion.FlaskApp(__name__, specification_dir='./openapi/')
    app.add_api('openapi.yaml',
                arguments={'title': 'OpenAPI Petstore'},
                pythonic_params=True,
                jsonifier=Jsonifier(cls=encoder.JSONEncoder))

    app.run(port=8080)


if __name__ == '__main__':
    main()
