from __future__ import absolute_import, division, print_function

import sys


__all__ = ["ABC", "text_type", "PY2"]

PY2 = sys.version_info[0] == 2


if PY2:
    import abc

    text_type = unicode  # noqa

    class ABC(object):
        __metaclass__ = abc.ABCMeta


else:
    from abc import ABC

    text_type = str
