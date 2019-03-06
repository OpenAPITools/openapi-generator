import abc
import logging
import pathlib
import sys

import six

from ..exceptions import ResolverError
from ..operations import make_operation
from ..options import ConnexionOptions
from ..resolver import Resolver
from ..spec import Specification
from ..utils import Jsonifier

MODULE_PATH = pathlib.Path(__file__).absolute().parent.parent
SWAGGER_UI_URL = 'ui'

logger = logging.getLogger('connexion.apis.abstract')


class AbstractAPIMeta(abc.ABCMeta):

    def __init__(cls, name, bases, attrs):
        abc.ABCMeta.__init__(cls, name, bases, attrs)
        cls._set_jsonifier()


@six.add_metaclass(AbstractAPIMeta)
class AbstractAPI(object):
    """
    Defines an abstract interface for a Swagger API
    """

    def __init__(self, specification, base_path=None, arguments=None,
                 validate_responses=False, strict_validation=False, resolver=None,
                 auth_all_paths=False, debug=False, resolver_error_handler=None,
                 validator_map=None, pythonic_params=False, pass_context_arg_name=None, options=None):
        """
        :type specification: pathlib.Path | dict
        :type base_path: str | None
        :type arguments: dict | None
        :type validate_responses: bool
        :type strict_validation: bool
        :type auth_all_paths: bool
        :type debug: bool
        :param validator_map: Custom validators for the types "parameter", "body" and "response".
        :type validator_map: dict
        :param resolver: Callable that maps operationID to a function
        :param resolver_error_handler: If given, a callable that generates an
            Operation used for handling ResolveErrors
        :type resolver_error_handler: callable | None
        :param pythonic_params: When True CamelCase parameters are converted to snake_case and an underscore is appended
        to any shadowed built-ins
        :type pythonic_params: bool
        :param options: New style options dictionary.
        :type options: dict | None
        :param pass_context_arg_name: If not None URL request handling functions with an argument matching this name
        will be passed the framework's request context.
        :type pass_context_arg_name: str | None
        """
        self.debug = debug
        self.validator_map = validator_map
        self.resolver_error_handler = resolver_error_handler

        logger.debug('Loading specification: %s', specification,
                     extra={'swagger_yaml': specification,
                            'base_path': base_path,
                            'arguments': arguments,
                            'auth_all_paths': auth_all_paths})

        # Avoid validator having ability to modify specification
        self.specification = Specification.load(specification, arguments=arguments)

        logger.debug('Read specification', extra={'spec': self.specification})

        self.options = ConnexionOptions(options, oas_version=self.specification.version)

        logger.debug('Options Loaded',
                     extra={'swagger_ui': self.options.openapi_console_ui_available,
                            'swagger_path': self.options.openapi_console_ui_from_dir,
                            'swagger_url': self.options.openapi_console_ui_path})

        self._set_base_path(base_path)

        logger.debug('Security Definitions: %s', self.specification.security_definitions)

        self.resolver = resolver or Resolver()

        logger.debug('Validate Responses: %s', str(validate_responses))
        self.validate_responses = validate_responses

        logger.debug('Strict Request Validation: %s', str(validate_responses))
        self.strict_validation = strict_validation

        logger.debug('Pythonic params: %s', str(pythonic_params))
        self.pythonic_params = pythonic_params

        logger.debug('pass_context_arg_name: %s', pass_context_arg_name)
        self.pass_context_arg_name = pass_context_arg_name

        if self.options.openapi_spec_available:
            self.add_openapi_json()

        if self.options.openapi_console_ui_available:
            self.add_swagger_ui()

        self.add_paths()

        if auth_all_paths:
            self.add_auth_on_not_found(
                self.specification.security,
                self.specification.security_definitions
            )

    def _set_base_path(self, base_path=None):
        if base_path is not None:
            # update spec to include user-provided base_path
            self.specification.base_path = base_path
            self.base_path = base_path
        else:
            self.base_path = self.specification.base_path

    @abc.abstractmethod
    def add_openapi_json(self):
        """
        Adds openapi spec to {base_path}/openapi.json
             (or {base_path}/swagger.json for swagger2)
        """

    @abc.abstractmethod
    def add_swagger_ui(self):
        """
        Adds swagger ui to {base_path}/ui/
        """

    @abc.abstractmethod
    def add_auth_on_not_found(self, security, security_definitions):
        """
        Adds a 404 error handler to authenticate and only expose the 404 status if the security validation pass.
        """

    def add_operation(self, path, method):
        """
        Adds one operation to the api.

        This method uses the OperationID identify the module and function that will handle the operation

        From Swagger Specification:

        **OperationID**

        A friendly name for the operation. The id MUST be unique among all operations described in the API.
        Tools and libraries MAY use the operation id to uniquely identify an operation.

        :type method: str
        :type path: str
        """
        operation = make_operation(
            self.specification,
            self,
            path,
            method,
            self.resolver,
            validate_responses=self.validate_responses,
            validator_map=self.validator_map,
            strict_validation=self.strict_validation,
            pythonic_params=self.pythonic_params,
            uri_parser_class=self.options.uri_parser_class,
            pass_context_arg_name=self.pass_context_arg_name
        )
        self._add_operation_internal(method, path, operation)

    @abc.abstractmethod
    def _add_operation_internal(self, method, path, operation):
        """
        Adds the operation according to the user framework in use.
        It will be used to register the operation on the user framework router.
        """

    def _add_resolver_error_handler(self, method, path, err):
        """
        Adds a handler for ResolverError for the given method and path.
        """
        operation = self.resolver_error_handler(
            err,
            security=self.specification.security,
            security_definitions=self.specification.security_definitions
        )
        self._add_operation_internal(method, path, operation)

    def add_paths(self, paths=None):
        """
        Adds the paths defined in the specification as endpoints

        :type paths: list
        """
        paths = paths or self.specification.get('paths', dict())
        for path, methods in paths.items():
            logger.debug('Adding %s%s...', self.base_path, path)

            for method in methods:
                if method == 'parameters':
                    continue
                try:
                    self.add_operation(path, method)
                except ResolverError as err:
                    # If we have an error handler for resolver errors, add it as an operation.
                    # Otherwise treat it as any other error.
                    if self.resolver_error_handler is not None:
                        self._add_resolver_error_handler(method, path, err)
                    else:
                        self._handle_add_operation_error(path, method, err.exc_info)
                except Exception:
                    # All other relevant exceptions should be handled as well.
                    self._handle_add_operation_error(path, method, sys.exc_info())

    def _handle_add_operation_error(self, path, method, exc_info):
        url = '{base_path}{path}'.format(base_path=self.base_path, path=path)
        error_msg = 'Failed to add operation for {method} {url}'.format(
            method=method.upper(),
            url=url)
        if self.debug:
            logger.exception(error_msg)
        else:
            logger.error(error_msg)
            six.reraise(*exc_info)

    @classmethod
    @abc.abstractmethod
    def get_request(self, *args, **kwargs):
        """
        This method converts the user framework request to a ConnexionRequest.
        """

    @classmethod
    @abc.abstractmethod
    def get_response(self, response, mimetype=None, request=None):
        """
        This method converts the ConnexionResponse to a user framework response.
        :param response: A response to cast.
        :param mimetype: The response mimetype.
        :param request: The request associated with this response (the user framework request).

        :type response: ConnexionResponse
        :type mimetype: str
        """

    @classmethod
    @abc.abstractmethod
    def get_connexion_response(cls, response):
        """
        This method converts the user framework response to a ConnexionResponse.
        :param response: A response to cast.
        """

    def json_loads(self, data):
        return self.jsonifier.loads(data)

    @classmethod
    def _set_jsonifier(cls):
        import json
        cls.jsonifier = Jsonifier(json)
