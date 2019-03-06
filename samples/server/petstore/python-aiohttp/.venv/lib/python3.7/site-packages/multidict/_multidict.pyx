from __future__ import absolute_import

import sys
from collections import abc
from collections.abc import Iterable, Set

from cpython.object cimport PyObject_Str, Py_NE, PyObject_RichCompare

from ._abc import MultiMapping, MutableMultiMapping
from ._istr import istr

from ._multidict_iter cimport *
from ._multidict_views cimport *
from ._pair_list cimport *

cdef object _marker = object()

upstr = istr  # for relaxing backward compatibility problems
cdef object _istr = istr

pair_list_init(istr)
# multidict_iter_init()
multidict_views_init()

def getversion(_Base md):
    return pair_list_version(md._impl)


cdef class _Base:

    cdef object _impl

    def impl(self):
        return self._impl

    def getall(self, key, default=_marker):
        """Return a list of all values matching the key."""
        try:
            return pair_list_get_all(self._impl, key)
        except KeyError:
            if default is not _marker:
                return default
            else:
                raise

    def getone(self, key, default=_marker):
        """Get first value matching the key."""
        return self._getone(key, default)

    cdef _getone(self, key, default):
        try:
            return pair_list_get_one(self._impl, key)
        except KeyError:
            if default is not _marker:
                return default
            else:
                raise

    # Mapping interface #

    def __getitem__(self, key):
        return self._getone(key, _marker)

    def get(self, key, default=None):
        """Get first value matching the key.

        The method is alias for .getone().
        """
        return self._getone(key, default)

    def __contains__(self, key):
        return self._contains(key)

    cdef _contains(self, key):
        return pair_list_contains(self._impl, key)

    def __iter__(self):
        return iter(self.keys())

    def __len__(self):
        return pair_list_len(self._impl)

    cpdef keys(self):
        """Return a new view of the dictionary's keys."""
        return multidict_keysview_new(self)

    def items(self):
        """Return a new view of the dictionary's items *(key, value) pairs)."""
        return multidict_itemsview_new(self)

    def values(self):
        """Return a new view of the dictionary's values."""
        return multidict_valuesview_new(self)

    def __repr__(self):
        lst = []
        for k, v in self.items():
            lst.append("'{}': {!r}".format(k, v))
        body = ', '.join(lst)
        return '<{}({})>'.format(self.__class__.__name__, body)

    def __eq__(self, arg):
        cdef Py_ssize_t pos1
        cdef PyObject *identity1
        cdef PyObject *value1
        cdef Py_hash_t h1

        cdef Py_ssize_t pos2
        cdef PyObject *identity2
        cdef PyObject *value2
        cdef Py_hash_t h2

        cdef _Base other

        if isinstance(arg, _Base):
            other = <_Base>arg
            if pair_list_len(self._impl) != pair_list_len(other._impl):
                return False
            pos1 = pos2 = 0
            while (_pair_list_next(self._impl, &pos1, &identity1,
                                   NULL, &value1, &h1) and
                   _pair_list_next(other._impl, &pos2, &identity2,
                                   NULL, &value2, &h2)):
                if h1 != h2:
                    return False
                if PyObject_RichCompare(<object>identity1, <object>identity2, Py_NE):
                    return False
                if PyObject_RichCompare(<object>value1, <object>value2, Py_NE):
                    return False
            return True
        elif isinstance(arg, abc.Mapping):
            return bool(pair_list_eq_to_mapping(self._impl, arg))
        else:
            return NotImplemented


cdef class MultiDictProxy(_Base):
    _proxy_classes = (MultiDict, MultiDictProxy)
    _base_class = MultiDict

    def __init__(self, arg):
        cdef _Base base
        if not isinstance(arg, self._proxy_classes):
            raise TypeError(
                'ctor requires {} instance'
                ', not {}'.format(
                    ' or '.join(self._proxy_classes),
                    type(arg)))

        base = arg
        self._impl = base._impl

    def __reduce__(self):
        raise TypeError("can't pickle {} objects"
                        .format(self.__class__.__name__))

    def copy(self):
        """Return a copy of itself."""
        return self._base_class(self)

MultiMapping.register(MultiDictProxy)


cdef class CIMultiDictProxy(MultiDictProxy):
    _proxy_classes = (CIMultiDict, CIMultiDictProxy)
    _base_class = CIMultiDict


MultiMapping.register(CIMultiDictProxy)


cdef str _str(key):
    typ = type(key)
    if typ is str:
        return <str>key
    if typ is _istr:
        return PyObject_Str(key)
    elif issubclass(typ, str):
        return str(key)
    else:
        raise TypeError("MultiDict keys should be either str "
                        "or subclasses of str")


cdef class MultiDict(_Base):
    """An ordered dictionary that can have multiple values for each key."""

    def __init__(self, *args, **kwargs):
        self._impl = pair_list_new()
        self._extend(args, kwargs, 'MultiDict', True)

    def __reduce__(self):
        return (
            self.__class__,
            (list(self.items()),)
        )

    cdef _extend(self, tuple args, dict kwargs, name, bint do_add):
        cdef object key
        cdef object value
        cdef object arg
        cdef object i

        if len(args) > 1:
            raise TypeError("{} takes at most 1 positional argument"
                            " ({} given)".format(name, len(args)))

        if args:
            arg = args[0]
            if isinstance(arg, _Base) and not kwargs:
                if do_add:
                    self._append_items((<_Base>arg)._impl)
                else:
                    self._update_items((<_Base>arg)._impl)
            else:
                if hasattr(arg, 'items'):
                    arg = arg.items()
                if kwargs:
                    arg = list(arg)
                    arg.extend(list(kwargs.items()))
                if do_add:
                    self._append_items_seq(arg, name)
                else:
                    pair_list_update_from_seq(self._impl, arg)
        else:
            arg = list(kwargs.items())
            if do_add:
                self._append_items_seq(arg, name)
            else:
                pair_list_update_from_seq(self._impl, arg)

    cdef object _update_items(self, object impl):
        pair_list_update(self._impl, impl)

    cdef object _append_items(self, object impl):
        cdef PyObject *key
        cdef PyObject *val
        cdef Py_ssize_t pos
        pos = 0
        while _pair_list_next(impl, &pos, NULL, &key, &val, NULL):
            self._add(<object>key, <object>val)

    cdef object _append_items_seq(self, object arg, object name):
        cdef object i
        cdef object key
        cdef object value
        for i in arg:
            if not len(i) == 2:
                raise TypeError(
                    "{} takes either dict or list of (key, value) "
                    "tuples".format(name))
            key = i[0]
            value = i[1]
            self._add(key, value)

    cdef _add(self, key, value):
        pair_list_add(self._impl, key, value);

    cdef _replace(self, key, value):
        pair_list_replace(self._impl, key, value)

    def add(self, key, value):
        """Add the key and value, not overwriting any previous value."""
        self._add(key, value)

    def copy(self):
        """Return a copy of itself."""
        ret = MultiDict()
        ret._extend((list(self.items()),), {}, 'copy', True)
        return ret

    def extend(self, *args, **kwargs):
        """Extend current MultiDict with more values.

        This method must be used instead of update.
        """
        self._extend(args, kwargs, "extend", True)

    def clear(self):
        """Remove all items from MultiDict"""
        pair_list_clear(self._impl)

    # MutableMapping interface #

    def __setitem__(self, key, value):
        self._replace(key, value)

    def __delitem__(self, key):
        pair_list_del(self._impl, key)

    def setdefault(self, key, default=None):
        """Return value for key, set value to default if key is not present."""
        return pair_list_set_default(self._impl, key, default)

    def popone(self, key, default=_marker):
        """Remove the last occurrence of key and return the corresponding
        value.

        If key is not found, default is returned if given, otherwise
        KeyError is raised.

        """
        try:
            return pair_list_pop_one(self._impl, key)
        except KeyError:
            if default is _marker:
                raise
            else:
                return default

    pop = popone

    def popall(self, key, default=_marker):
        """Remove all occurrences of key and return the list of corresponding
        values.

        If key is not found, default is returned if given, otherwise
        KeyError is raised.

        """
        try:
            return pair_list_pop_all(self._impl, key)
        except KeyError:
            if default is _marker:
                raise
            else:
                return default

    def popitem(self):
        """Remove and return an arbitrary (key, value) pair."""
        return pair_list_pop_item(self._impl)

    def update(self, *args, **kwargs):
        """Update the dictionary from *other*, overwriting existing keys."""
        self._extend(args, kwargs, "update", False)


MutableMultiMapping.register(MultiDict)


cdef class CIMultiDict(MultiDict):
    """An ordered dictionary that can have multiple values for each key."""

    def __init__(self, *args, **kwargs):
        self._impl = ci_pair_list_new()
        self._extend(args, kwargs, 'CIMultiDict', True)

    def __reduce__(self):
        return (
            self.__class__,
            (list(self.items()),),
        )

    def copy(self):
        """Return a copy of itself."""
        ret = CIMultiDict()
        ret._extend((list(self.items()),), {}, 'copy', True)
        return ret


MutableMultiMapping.register(CIMultiDict)

