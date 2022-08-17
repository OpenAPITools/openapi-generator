from petstore_api.paths.fake_2.get import ApiForget
from petstore_api.paths.fake_2.post import ApiForpost
from petstore_api.paths.fake_2.delete import ApiFordelete
from petstore_api.paths.fake_2.patch import ApiForpatch


class Fake(
    ApiForget,
    ApiForpost,
    ApiFordelete,
    ApiForpatch,
):
    pass
