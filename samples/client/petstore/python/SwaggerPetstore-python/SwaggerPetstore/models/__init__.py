#!/usr/bin/env python
"""Add all of the modules in the current directory to __all__"""
from __future__ import absolute_import

import os


from .user import User

from .category import Category

from .pet import Pet

from .tag import Tag

from .order import Order


__all__ = []

for module in os.listdir(os.path.dirname(__file__)):
  if module != '__init__.py' and module[-3:] == '.py':
    __all__.append(module[:-3])
