# coding: utf-8

from six import iteritems

def remove_none(obj):
    """
    Remove None from `list`, `tuple`, `set`.
    Remove None value from `dict`.
    """
    if isinstance(obj, (list, tuple, set)):
        return type(obj)(remove_none(x) for x in obj if x is not None)
    elif isinstance(obj, dict):
        return type(obj)((remove_none(k), remove_none(v))
                         for k, v in iteritems(obj) if k is not None and v is not None)
    else:
        return obj
