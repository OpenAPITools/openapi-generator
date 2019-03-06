import decimal
import hmac
import numbers
import sys

PY2 = sys.version_info[0] == 2

if PY2:
    from itertools import izip

    text_type = unicode  # noqa: 821
else:
    izip = zip
    text_type = str

number_types = (numbers.Real, decimal.Decimal)


def _constant_time_compare(val1, val2):
    """Return ``True`` if the two strings are equal, ``False``
    otherwise.

    The time taken is independent of the number of characters that
    match. Do not use this function for anything else than comparision
    with known length targets.

    This is should be implemented in C in order to get it completely
    right.

    This is an alias of :func:`hmac.compare_digest` on Python>=2.7,3.3.
    """
    len_eq = len(val1) == len(val2)
    if len_eq:
        result = 0
        left = val1
    else:
        result = 1
        left = val2
    for x, y in izip(bytearray(left), bytearray(val2)):
        result |= x ^ y
    return result == 0


# Starting with 2.7/3.3 the standard library has a c-implementation for
# constant time string compares.
constant_time_compare = getattr(hmac, "compare_digest", _constant_time_compare)
