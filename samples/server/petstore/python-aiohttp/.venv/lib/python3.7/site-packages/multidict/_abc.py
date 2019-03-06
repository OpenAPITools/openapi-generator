import abc

from collections.abc import Mapping, MutableMapping


class _TypingMeta(abc.ABCMeta):
    # A fake metaclass to satisfy typing deps in runtime
    # basically MultiMapping[str] and other generic-like type instantiations
    # are emulated.
    # Note: real type hints are provided by __init__.pyi stub file
    def __getitem__(self, key):
        return self


class MultiMapping(Mapping, metaclass=_TypingMeta):

    @abc.abstractmethod
    def getall(self, key, default=None):
        raise KeyError

    @abc.abstractmethod
    def getone(self, key, default=None):
        raise KeyError


class MutableMultiMapping(MultiMapping, MutableMapping):

    @abc.abstractmethod
    def add(self, key, value):
        raise NotImplementedError

    @abc.abstractmethod
    def extend(self, *args, **kwargs):
        raise NotImplementedError

    @abc.abstractmethod
    def popone(self, key, default=None):
        raise KeyError

    @abc.abstractmethod
    def popall(self, key, default=None):
        raise KeyError
