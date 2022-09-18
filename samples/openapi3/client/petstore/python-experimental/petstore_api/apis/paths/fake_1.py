from petstore_api.paths.fake_1.get import ApiForget
from petstore_api.paths.fake_1.post import ApiForpost
from petstore_api.paths.fake_1.delete import ApiFordelete
from petstore_api.paths.fake_1.patch import ApiForpatch


class Fake(
    ApiForget,
    ApiForpost,
    ApiFordelete,
    ApiForpatch,
):
    pass
