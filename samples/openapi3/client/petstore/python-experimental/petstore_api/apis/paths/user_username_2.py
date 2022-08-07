from petstore_api.paths.user_username_2.get import ApiForget
from petstore_api.paths.user_username_2.put import ApiForput
from petstore_api.paths.user_username_2.delete import ApiFordelete


class UserUsername(
    ApiForget,
    ApiForput,
    ApiFordelete,
):
    pass
