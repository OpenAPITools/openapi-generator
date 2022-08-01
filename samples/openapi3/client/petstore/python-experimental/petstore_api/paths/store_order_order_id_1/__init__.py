# do not import all endpoints into this module because that uses a lot of memory and stack frames
# if you need the ability to import all endpoints from this module, import them with
# from petstore_api.paths.store_order_order_id_1 import Api

from petstore_api.paths import PathValues

path = PathValues.STORE_ORDER_ORDER_ID