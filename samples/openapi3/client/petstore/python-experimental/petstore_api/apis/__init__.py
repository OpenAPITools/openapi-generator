# coding: utf-8

# flake8: noqa

# import all apis into this package
# if you have many ampis here with many many models used in each api this may
# raise a RecursionError
# to avoid this, import only the api that you directly need like:
# from .api.pet_api import PetApi
# or import this package, but before doing it, use:
# import sys
# sys.setrecursionlimit(n)

# import apis into api package
from petstore_api.api.another_fake_api import AnotherFakeApi
from petstore_api.api.default_api import DefaultApi
from petstore_api.api.fake_api import FakeApi
from petstore_api.api.fake_classname_tags_123_api import FakeClassnameTags123Api
from petstore_api.api.pet_api import PetApi
from petstore_api.api.store_api import StoreApi
from petstore_api.api.user_api import UserApi
