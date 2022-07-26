from petstore_api.path.user_username.get import ApiForget
from petstore_api.path.user_username.put import ApiForput
from petstore_api.path.user_username.delete import ApiFordelete

class Api(
    ApiForget,
    ApiForput,
    ApiFordelete,
):
    pass
