"""OpenAPI spec validator handlers module."""
import contextlib

from six.moves.urllib.parse import urlparse
from six.moves.urllib.request import urlopen
from yaml import load

from openapi_spec_validator.loaders import ExtendedSafeLoader


class UrlHandler:
    """OpenAPI spec validator URL scheme handler."""

    def __init__(self, *allowed_schemes, **options):
        self.allowed_schemes = allowed_schemes
        self.options = options

    @property
    def loader(self):
        return self.options.get('loader', ExtendedSafeLoader)

    def __call__(self, url, timeout=1):
        assert urlparse(url).scheme in self.allowed_schemes

        with contextlib.closing(urlopen(url, timeout=timeout)) as fh:
            return load(fh, self.loader)
