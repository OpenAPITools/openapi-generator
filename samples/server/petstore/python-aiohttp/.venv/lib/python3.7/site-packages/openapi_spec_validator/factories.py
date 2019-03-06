"""OpenAPI spec validator factories module."""
from jsonschema import validators
from jsonschema.validators import Draft4Validator, RefResolver

from openapi_spec_validator.generators import (
    SpecValidatorsGeneratorFactory,
)


class Draft4ExtendedValidatorFactory(Draft4Validator):
    """Draft4Validator with extra validators factory that follows $refs
    in the schema being validated."""

    @classmethod
    def from_resolver(cls, spec_resolver):
        """Creates a customized Draft4ExtendedValidator.

        :param spec_resolver: resolver for the spec
        :type resolver: :class:`jsonschema.RefResolver`
        """
        spec_validators = cls._get_spec_validators(spec_resolver)
        return validators.extend(Draft4Validator, spec_validators)

    @classmethod
    def _get_spec_validators(cls, spec_resolver):
        generator = SpecValidatorsGeneratorFactory.from_spec_resolver(
            spec_resolver)
        return dict(list(generator))


class JSONSpecValidatorFactory:
    """
    Json documents validator factory against a json schema.

    :param schema: schema for validation.
    :param schema_url: schema base uri.
    """

    schema_validator_class = Draft4Validator
    spec_validator_factory = Draft4ExtendedValidatorFactory

    def __init__(self, schema, schema_url='', resolver_handlers=None):
        self.schema = schema
        self.schema_url = schema_url
        self.resolver_handlers = resolver_handlers or ()

        self.schema_validator_class.check_schema(self.schema)

    @property
    def schema_resolver(self):
        return self._get_resolver(self.schema_url, self.schema)

    def create(self, spec_resolver):
        """Creates json documents validator from spec resolver.
        :param spec_resolver: reference resolver.

        :return: RefResolver for spec with cached remote $refs used during
            validation.
        :rtype: :class:`jsonschema.RefResolver`
        """
        validator_cls = self.spec_validator_factory.from_resolver(
            spec_resolver)

        return validator_cls(
            self.schema, resolver=self.schema_resolver)

    def _get_resolver(self, base_uri, referrer):
        return RefResolver(
            base_uri, referrer, handlers=self.resolver_handlers)
