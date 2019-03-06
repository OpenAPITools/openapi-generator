from yaml.composer import Composer
from yaml.parser import Parser
from yaml.reader import Reader
from yaml.resolver import Resolver
from yaml.scanner import Scanner

from openapi_spec_validator.constructors import ExtendedSafeConstructor


class ExtendedSafeLoader(
        Reader, Scanner, Parser, Composer, ExtendedSafeConstructor, Resolver):

    def __init__(self, stream):
        Reader.__init__(self, stream)
        Scanner.__init__(self)
        Parser.__init__(self)
        Composer.__init__(self)
        ExtendedSafeConstructor.__init__(self)
        Resolver.__init__(self)
