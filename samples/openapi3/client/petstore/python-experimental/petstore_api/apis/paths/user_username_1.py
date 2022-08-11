from petstore_api.paths.user_username_1.get import ApiForget
from petstore_api.paths.user_username_1.put import ApiForput
from petstore_api.paths.user_username_1.delete import ApiFordelete


class UserUsername(
    ApiForget,
    ApiForput,
    ApiFordelete,
):
    pass
