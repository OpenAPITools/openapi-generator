# coding: utf-8

# flake8: noqa

# Import all APIs into this package.
# If you have many APIs here with many many models used in each API this may
# raise a `RecursionError`.
# In order to avoid this, import only the API that you directly need like:
#
#   from unit_test_api.api.json_api import JsonApi
#
# or import this package, but before doing it, use:
#
#   import sys
#   sys.setrecursionlimit(n)

# Import APIs into API package:
from unit_test_api.api.json_api import JsonApi
from unit_test_api.api.post_api import PostApi
from unit_test_api.api.request_body_api import RequestBodyApi
