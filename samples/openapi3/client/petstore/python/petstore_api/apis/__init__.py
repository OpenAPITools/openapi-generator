
# flake8: noqa

# Import all APIs into this package.
# If you have many APIs here with many many models used in each API this may
# raise a `RecursionError`.
# In order to avoid this, import only the API that you directly need like:
#
#   from .api.another_fake_api import AnotherFakeApi
#
# or import this package, but before doing it, use:
#
#   import sys
#   sys.setrecursionlimit(n)

# Import APIs into API package:
from petstore_api.api.another_fake_api import AnotherFakeApi
from petstore_api.api.default_api import DefaultApi
from petstore_api.api.fake_api import FakeApi
from petstore_api.api.fake_classname_tags123_api import FakeClassnameTags123Api
from petstore_api.api.pet_api import PetApi
from petstore_api.api.store_api import StoreApi
from petstore_api.api.user_api import UserApi
