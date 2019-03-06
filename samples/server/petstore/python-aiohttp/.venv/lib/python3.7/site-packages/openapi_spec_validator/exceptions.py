from jsonschema.exceptions import ValidationError


class OpenAPIValidationError(ValidationError):
    pass


class ExtraParametersError(OpenAPIValidationError):
    pass


class ParameterDuplicateError(OpenAPIValidationError):
    pass


class UnresolvableParameterError(OpenAPIValidationError):
    pass
