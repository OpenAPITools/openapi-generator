"""OpenAPI spec validator generators module."""
import logging

from six import iteritems
from jsonschema import _validators

from openapi_spec_validator.decorators import DerefValidatorDecorator

log = logging.getLogger(__name__)


class SpecValidatorsGeneratorFactory:
    """Generator factory for customized validators that follows $refs
    in the schema being validated.
    """

    validators = {
        '$ref': _validators.ref,
        'properties': _validators.properties_draft4,
        'additionalProperties': _validators.additionalProperties,
        'patternProperties': _validators.patternProperties,
        'type': _validators.type_draft4,
        'dependencies': _validators.dependencies,
        'required': _validators.required_draft4,
        'minProperties': _validators.minProperties_draft4,
        'maxProperties': _validators.maxProperties_draft4,
        'allOf': _validators.allOf_draft4,
        'oneOf': _validators.oneOf_draft4,
        'anyOf': _validators.anyOf_draft4,
        'not': _validators.not_draft4,
    }

    @classmethod
    def from_spec_resolver(cls, spec_resolver):
        """Creates validators generator for the spec resolver.

        :param spec_resolver: resolver for the spec
        :type instance_resolver: :class:`jsonschema.RefResolver`
        """
        deref = DerefValidatorDecorator(spec_resolver)
        for key, validator_callable in iteritems(cls.validators):
            yield key, deref(validator_callable)
