# -*- coding: utf-8 -*-
from openapi_spec_validator.shortcuts import (
    validate_spec_factory, validate_spec_url_factory,
)
from openapi_spec_validator.handlers import UrlHandler
from openapi_spec_validator.schemas import get_openapi_schema
from openapi_spec_validator.factories import JSONSpecValidatorFactory
from openapi_spec_validator.validators import SpecValidator

__author__ = 'Artur Maciag'
__email__ = 'maciag.artur@gmail.com'
__version__ = '0.2.6'
__url__ = 'https://github.com/p1c2u/openapi-spec-validator'
__license__ = 'Apache License, Version 2.0'

__all__ = [
    'openapi_v2_spec_validator', 'openapi_v3_spec_validator',
    'validate_v2_spec', 'validate_v3_spec', 'validate_spec',
    'validate_v2_spec_url', 'validate_v3_spec_url', 'validate_spec_url',
]

default_handlers = {
    '<all_urls>': UrlHandler('http', 'https', 'file'),
    'http': UrlHandler('http'),
    'https': UrlHandler('https'),
    'file': UrlHandler('file'),
}

# v2.0 spec
schema_v2, schema_v2_url = get_openapi_schema('2.0')
openapi_v2_validator_factory = JSONSpecValidatorFactory(
    schema_v2, schema_v2_url,
    resolver_handlers=default_handlers,
)
openapi_v2_spec_validator = SpecValidator(
    openapi_v2_validator_factory,
    resolver_handlers=default_handlers,
)

# v3.0.0 spec
schema_v3, schema_v3_url = get_openapi_schema('3.0.0')
openapi_v3_validator_factory = JSONSpecValidatorFactory(
    schema_v3, schema_v3_url,
    resolver_handlers=default_handlers,
)
openapi_v3_spec_validator = SpecValidator(
    openapi_v3_validator_factory,
    resolver_handlers=default_handlers,
)

# shortcuts
validate_v2_spec = validate_spec_factory(openapi_v2_spec_validator.validate)
validate_v2_spec_url = validate_spec_url_factory(
    openapi_v2_spec_validator.validate, default_handlers)

validate_v3_spec = validate_spec_factory(openapi_v3_spec_validator.validate)
validate_v3_spec_url = validate_spec_url_factory(
    openapi_v3_spec_validator.validate, default_handlers)

# aliases to the latest version
validate_spec = validate_v3_spec
validate_spec_url = validate_v3_spec_url
