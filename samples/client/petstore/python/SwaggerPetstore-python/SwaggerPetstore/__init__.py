#!/usr/bin/env python
"""Add all of the modules in the current directory to __all__"""
import os

# import models into package

from .models.user import User

from .models.category import Category

from .models.pet import Pet

from .models.tag import Tag

from .models.order import Order


# import apis into package

from .user_api import UserApi

from .pet_api import PetApi

from .store_api import StoreApi


# import ApiClient
from .swagger import ApiClient

__all__ = []

for module in os.listdir(os.path.dirname(__file__)):
  if module != '__init__.py' and module[-3:] == '.py':
    __all__.append(module[:-3])
