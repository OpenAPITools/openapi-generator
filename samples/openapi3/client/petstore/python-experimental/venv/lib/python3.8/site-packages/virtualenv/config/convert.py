from __future__ import absolute_import, unicode_literals

import logging
import os


class TypeData(object):
    def __init__(self, default_type, as_type):
        self.default_type = default_type
        self.as_type = as_type

    def __repr__(self):
        return "{}(base={}, as={})".format(self.__class__.__name__, self.default_type, self.as_type)

    def convert(self, value):
        return self.default_type(value)


class BoolType(TypeData):
    BOOLEAN_STATES = {
        "1": True,
        "yes": True,
        "true": True,
        "on": True,
        "0": False,
        "no": False,
        "false": False,
        "off": False,
    }

    def convert(self, value):
        if value.lower() not in self.BOOLEAN_STATES:
            raise ValueError("Not a boolean: %s" % value)
        return self.BOOLEAN_STATES[value.lower()]


class NoneType(TypeData):
    def convert(self, value):
        if not value:
            return None
        return str(value)


class ListType(TypeData):
    def _validate(self):
        """"""

    def convert(self, value, flatten=True):
        if isinstance(value, (str, bytes)):
            value = filter(None, [x.strip() for x in value.splitlines()])
        values = list(value)
        result = []
        for value in values:
            sub_values = value.split(os.pathsep)
            result.extend(sub_values)
        converted = [self.as_type(i) for i in result]
        return converted


def convert(value, as_type, source):
    """Convert the value as a given type where the value comes from the given source"""
    try:
        return as_type.convert(value)
    except Exception as exception:
        logging.warning("%s failed to convert %r as %r because %r", source, value, as_type, exception)
        raise


_CONVERT = {bool: BoolType, type(None): NoneType, list: ListType}


def get_type(action):
    default_type = type(action.default)
    as_type = default_type if action.type is None else action.type
    return _CONVERT.get(default_type, TypeData)(default_type, as_type)


__all__ = (
    "convert",
    "get_type",
)
