from petstore_api.paths.fake_3.get import ApiForget
from petstore_api.paths.fake_3.post import ApiForpost
from petstore_api.paths.fake_3.delete import ApiFordelete
from petstore_api.paths.fake_3.patch import ApiForpatch


class Fake(
    ApiForget,
    ApiForpost,
    ApiFordelete,
    ApiForpatch,
):
    pass
