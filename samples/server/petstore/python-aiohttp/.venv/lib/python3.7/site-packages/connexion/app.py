import logging
import pathlib

import flask
import werkzeug.exceptions

from .api import Api
from .decorators.produces import JSONEncoder as ConnexionJSONEncoder
from .exceptions import ProblemException
from .problem import problem
from .resolver import Resolver

logger = logging.getLogger('connexion.app')


class App(object):
    def __init__(self, import_name, port=None, specification_dir='',
                 server=None, arguments=None, auth_all_paths=False,
                 debug=False, swagger_json=True, swagger_ui=True, swagger_path=None,
                 swagger_url=None, host=None, validator_map=None):
        """
        :param import_name: the name of the application package
        :type import_name: str
        :param host: the host interface to bind on.
        :type host: str
        :param port: port to listen to
        :type port: int
        :param specification_dir: directory where to look for specifications
        :type specification_dir: pathlib.Path | str
        :param server: which wsgi server to use
        :type server: str | None
        :param arguments: arguments to replace on the specification
        :type arguments: dict | None
        :param auth_all_paths: whether to authenticate not defined paths
        :type auth_all_paths: bool
        :param debug: include debugging information
        :type debug: bool
        :param swagger_json: whether to include swagger json or not
        :type swagger_json: bool
        :param swagger_ui: whether to include swagger ui or not
        :type swagger_ui: bool
        :param swagger_path: path to swagger-ui directory
        :type swagger_path: string | None
        :param swagger_url: URL to access swagger-ui documentation
        :type swagger_url: string | None
        :param validator_map: map of validators
        :type validator_map: dict
        """
        self.app = flask.Flask(import_name)

        self.app.json_encoder = ConnexionJSONEncoder

        # we get our application root path from flask to avoid duplicating logic
        self.root_path = pathlib.Path(self.app.root_path)
        logger.debug('Root Path: %s', self.root_path)

        specification_dir = pathlib.Path(specification_dir)  # Ensure specification dir is a Path
        if specification_dir.is_absolute():
            self.specification_dir = specification_dir
        else:
            self.specification_dir = self.root_path / specification_dir

        logger.debug('Specification directory: %s', self.specification_dir)

        logger.debug('Setting error handlers')
        for error_code in werkzeug.exceptions.default_exceptions:
            self.add_error_handler(error_code, self.common_error_handler)

        self.port = port
        self.host = host
        self.server = server or 'flask'
        self.debug = debug
        self.import_name = import_name
        self.arguments = arguments or {}
        self.swagger_json = swagger_json
        self.swagger_ui = swagger_ui
        self.swagger_path = swagger_path
        self.swagger_url = swagger_url
        self.auth_all_paths = auth_all_paths
        self.resolver_error = None
        self.validator_map = validator_map

    @staticmethod
    def common_error_handler(exception):
        """
        :type exception: Exception
        """
        if isinstance(exception, ProblemException):
            response_container = exception.to_problem()
        else:
            if not isinstance(exception, werkzeug.exceptions.HTTPException):
                exception = werkzeug.exceptions.InternalServerError()

            response_container = problem(title=exception.name, detail=exception.description,
                                         status=exception.code)

        return response_container.flask_response_object()

    def add_api(self, specification, base_path=None, arguments=None, auth_all_paths=None, swagger_json=None,
                swagger_ui=None, swagger_path=None, swagger_url=None, validate_responses=False,
                strict_validation=False, resolver=Resolver(), resolver_error=None):
        """
        Adds an API to the application based on a swagger file or API dict

        :param specification: swagger file with the specification | specification dict
        :type specification: pathlib.Path or dict
        :param base_path: base path where to add this api
        :type base_path: str | None
        :param arguments: api version specific arguments to replace on the specification
        :type arguments: dict | None
        :param auth_all_paths: whether to authenticate not defined paths
        :type auth_all_paths: bool
        :param swagger_json: whether to include swagger json or not
        :type swagger_json: bool
        :param swagger_ui: whether to include swagger ui or not
        :type swagger_ui: bool
        :param swagger_path: path to swagger-ui directory
        :type swagger_path: string | None
        :param swagger_url: URL to access swagger-ui documentation
        :type swagger_url: string | None
        :param validate_responses: True enables validation. Validation errors generate HTTP 500 responses.
        :type validate_responses: bool
        :param strict_validation: True enables validation on invalid request parameters
        :type strict_validation: bool
        :param resolver: Operation resolver.
        :type resolver: Resolver | types.FunctionType
        :param resolver_error: If specified, turns ResolverError into error
            responses with the given status code.
        :type resolver_error: int | None
        :rtype: Api
        """
        # Turn the resolver_error code into a handler object
        self.resolver_error = resolver_error
        resolver_error_handler = None
        if self.resolver_error is not None:
            resolver_error_handler = self._resolver_error_handler

        resolver = Resolver(resolver) if hasattr(resolver, '__call__') else resolver

        swagger_json = swagger_json if swagger_json is not None else self.swagger_json
        swagger_ui = swagger_ui if swagger_ui is not None else self.swagger_ui
        swagger_path = swagger_path if swagger_path is not None else self.swagger_path
        swagger_url = swagger_url if swagger_url is not None else self.swagger_url
        auth_all_paths = auth_all_paths if auth_all_paths is not None else self.auth_all_paths
        # TODO test if base_url starts with an / (if not none)
        arguments = arguments or dict()
        arguments = dict(self.arguments, **arguments)  # copy global arguments and update with api specfic

        if isinstance(specification, dict):
            specification = specification
        else:
            specification = self.specification_dir / specification

        api = Api(specification=specification,
                  base_url=base_path, arguments=arguments,
                  swagger_json=swagger_json,
                  swagger_ui=swagger_ui,
                  swagger_path=swagger_path,
                  swagger_url=swagger_url,
                  resolver=resolver,
                  resolver_error_handler=resolver_error_handler,
                  validate_responses=validate_responses,
                  strict_validation=strict_validation,
                  auth_all_paths=auth_all_paths,
                  debug=self.debug,
                  validator_map=self.validator_map)
        self.app.register_blueprint(api.blueprint)
        return api

    def _resolver_error_handler(self, *args, **kwargs):
        from connexion.handlers import ResolverErrorHandler
        kwargs['operation'] = {
            'operationId': 'connexion.handlers.ResolverErrorHandler',
        }
        kwargs.setdefault('app_consumes', ['application/json'])
        return ResolverErrorHandler(self.resolver_error, *args, **kwargs)

    def add_error_handler(self, error_code, function):
        """

        :type error_code: int
        :type function: types.FunctionType
        """
        self.app.register_error_handler(error_code, function)

    def add_url_rule(self, rule, endpoint=None, view_func=None, **options):
        """
        Connects a URL rule.  Works exactly like the `route` decorator.  If a view_func is provided it will be
        registered with the endpoint.

        Basically this example::

            @app.route('/')
            def index():
                pass

        Is equivalent to the following::

            def index():
                pass
            app.add_url_rule('/', 'index', index)

        If the view_func is not provided you will need to connect the endpoint to a view function like so::

            app.view_functions['index'] = index

        Internally`route` invokes `add_url_rule` so if you want to customize the behavior via subclassing you only need
        to change this method.

        :param rule: the URL rule as string
        :type rule: str
        :param endpoint: the endpoint for the registered URL rule. Flask itself assumes the name of the view function as
                         endpoint
        :type endpoint: str
        :param view_func: the function to call when serving a request to the provided endpoint
        :type view_func: types.FunctionType
        :param options: the options to be forwarded to the underlying `werkzeug.routing.Rule` object.  A change
                        to Werkzeug is handling of method options. methods is a list of methods this rule should be
                        limited to (`GET`, `POST` etc.).  By default a rule just listens for `GET` (and implicitly
                        `HEAD`).
        """
        log_details = {'endpoint': endpoint, 'view_func': view_func.__name__}
        log_details.update(options)
        logger.debug('Adding %s', rule, extra=log_details)
        self.app.add_url_rule(rule, endpoint, view_func, **options)

    def route(self, rule, **options):
        """
        A decorator that is used to register a view function for a
        given URL rule.  This does the same thing as `add_url_rule`
        but is intended for decorator usage::

            @app.route('/')
            def index():
                return 'Hello World'

        :param rule: the URL rule as string
        :type rule: str
        :param endpoint: the endpoint for the registered URL rule.  Flask
                         itself assumes the name of the view function as
                         endpoint
        :param options: the options to be forwarded to the underlying `werkzeug.routing.Rule` object.  A change
                        to Werkzeug is handling of method options.  methods is a list of methods this rule should be
                        limited to (`GET`, `POST` etc.).  By default a rule just listens for `GET` (and implicitly
                        `HEAD`).
        """
        logger.debug('Adding %s with decorator', rule, extra=options)
        return self.app.route(rule, **options)

    def run(self, port=None, server=None, debug=None, host=None, **options):  # pragma: no cover
        """
        Runs the application on a local development server.
        :param host: the host interface to bind on.
        :type host: str
        :param port: port to listen to
        :type port: int
        :param server: which wsgi server to use
        :type server: str | None
        :param debug: include debugging information
        :type debug: bool
        :param options: options to be forwarded to the underlying server
        :type options: dict
        """
        # this functions is not covered in unit tests because we would effectively testing the mocks

        # overwrite constructor parameter
        if port is not None:
            self.port = port
        elif self.port is None:
            self.port = 5000

        self.host = host or self.host or '0.0.0.0'

        if server is not None:
            self.server = server

        if debug is not None:
            self.debug = debug

        logger.debug('Starting %s HTTP server..', self.server, extra=vars(self))
        if self.server == 'flask':
            self.app.run(self.host, port=self.port, debug=self.debug, **options)
        elif self.server == 'tornado':
            try:
                import tornado.wsgi
                import tornado.httpserver
                import tornado.ioloop
            except:
                raise Exception('tornado library not installed')
            wsgi_container = tornado.wsgi.WSGIContainer(self.app)
            http_server = tornado.httpserver.HTTPServer(wsgi_container, **options)
            http_server.listen(self.port, address=self.host)
            logger.info('Listening on %s:%s..', self.host, self.port)
            tornado.ioloop.IOLoop.instance().start()
        elif self.server == 'gevent':
            try:
                import gevent.wsgi
            except:
                raise Exception('gevent library not installed')
            http_server = gevent.wsgi.WSGIServer((self.host, self.port), self.app, **options)
            logger.info('Listening on %s:%s..', self.host, self.port)
            http_server.serve_forever()
        else:
            raise Exception('Server %s not recognized', self.server)

    def __call__(self, environ, start_response):  # pragma: no cover
        """
        Makes the class callable to be WSGI-compliant. As Flask is used to handle requests,
        this is a passthrough-call to the Flask callable class.
        This is an abstraction to avoid directly referencing the app attribute from outside the
        class and protect it from unwanted modification.
        """
        return self.app(environ, start_response)
