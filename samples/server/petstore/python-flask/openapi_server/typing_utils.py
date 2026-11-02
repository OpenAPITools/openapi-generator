
def is_generic(klass):
    """Determine whether klass is a generic class."""
    return hasattr(klass, '__origin__')


def is_dict(klass):
    """Determine whether klass is a Dict."""
    return getattr(klass, '__origin__', None) is dict


def is_list(klass):
    """Determine whether klass is a List."""
    return getattr(klass, '__origin__', None) is list
