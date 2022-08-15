from petstore_api.paths.user_username.get import ApiForget
from petstore_api.paths.user_username.put import ApiForput
from petstore_api.paths.user_username.delete import ApiFordelete


class UserUsername(
    ApiForget,
    ApiForput,
    ApiFordelete,
):
    pass
