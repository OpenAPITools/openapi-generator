from six import iteritems

def remove_none(obj):
    if isinstance(obj, (list, tuple, set)):
        return type(obj)(remove_none(x) for x in obj if x is not None)
    elif isinstance(obj, dict):
        return type(obj)((remove_none(k), remove_none(v))
                         for k, v in iteritems(obj) if k is not None and v is not None)
    else:
        return obj


def inspect_vars(obj):
    if not hasattr(obj, '__dict__'):
        return obj
    else:
        return {k: inspect_vars(getattr(obj, k)) for k in dir(obj)}
