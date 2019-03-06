"""OpenAIP spec validator schemas module."""
import os

from pkg_resources import resource_filename
from six.moves.urllib import parse, request
from yaml import load

from openapi_spec_validator.loaders import ExtendedSafeLoader


def get_openapi_schema(version):
    path = 'resources/schemas/v{0}/schema.json'.format(version)
    path_resource = resource_filename('openapi_spec_validator', path)
    path_full = os.path.join(os.path.dirname(__file__), path_resource)
    schema = read_yaml_file(path_full)
    schema_url = parse.urljoin('file:', request.pathname2url(path_full))
    return schema, schema_url


def read_yaml_file(path, loader=ExtendedSafeLoader):
    """Open a file, read it and return its contents."""
    with open(path) as fh:
        return load(fh, loader)
