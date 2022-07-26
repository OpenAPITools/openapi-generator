from petstore_api.path.store_order_order_id.get import ApiForget
from petstore_api.path.store_order_order_id.delete import ApiFordelete

class Api(
    ApiForget,
    ApiFordelete,
):
    pass
