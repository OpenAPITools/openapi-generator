#!/usr/bin/env python3

import connexion

from openapi_server import encoder


def main():
    app = connexion.App(__name__, specification_dir='./openapi/')
    app.app.json_encoder = encoder.JSONEncoder

    # Switch needed to make the error messages sensible:
    # Any missing mandatory query params will throw a fault that includes the name of the missing param.
    # However, for some reason form params are treated entirely differently and don't include 
    # the missing param name in the fault and the default error response is totally different too.
    # Setting the flag below at least ensures that the error message contains the missing param name 
    # which can save a ton of time trying to figure out issues.
    # See https://github.com/pallets/flask/issues/3249 
    app.app.config['TRAP_BAD_REQUEST_ERRORS'] = True 

    app.add_api('openapi.yaml',
                arguments={'title': 'OpenAPI Enums Exemplar'},
                pythonic_params=True)

    app.run(port=8080)


if __name__ == '__main__':
    main()
