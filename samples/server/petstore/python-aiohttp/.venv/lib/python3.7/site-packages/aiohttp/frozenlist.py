from collections.abc import MutableSequence
from functools import total_ordering

from .helpers import NO_EXTENSIONS


@total_ordering
class FrozenList(MutableSequence):

    __slots__ = ('_frozen', '_items')

    def __init__(self, items=None):
        self._frozen = False
        if items is not None:
            items = list(items)
        else:
            items = []
        self._items = items

    @property
    def frozen(self):
        return self._frozen

    def freeze(self):
        self._frozen = True

    def __getitem__(self, index):
        return self._items[index]

    def __setitem__(self, index, value):
        if self._frozen:
            raise RuntimeError("Cannot modify frozen list.")
        self._items[index] = value

    def __delitem__(self, index):
        if self._frozen:
            raise RuntimeError("Cannot modify frozen list.")
        del self._items[index]

    def __len__(self):
        return self._items.__len__()

    def __iter__(self):
        return self._items.__iter__()

    def __reversed__(self):
        return self._items.__reversed__()

    def __eq__(self, other):
        return list(self) == other

    def __le__(self, other):
        return list(self) <= other

    def insert(self, pos, item):
        if self._frozen:
            raise RuntimeError("Cannot modify frozen list.")
        self._items.insert(pos, item)

    def __repr__(self):
        return '<FrozenList(frozen={}, {!r})>'.format(self._frozen,
                                                      self._items)


PyFrozenList = FrozenList

try:
    from aiohttp._frozenlist import FrozenList as CFrozenList  # type: ignore
    if not NO_EXTENSIONS:
        FrozenList = CFrozenList  # type: ignore
except ImportError:  # pragma: no cover
    pass
