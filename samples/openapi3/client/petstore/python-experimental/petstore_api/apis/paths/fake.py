from petstore_api.paths.fake.get import ApiForget
from petstore_api.paths.fake.post import ApiForpost
from petstore_api.paths.fake.delete import ApiFordelete
from petstore_api.paths.fake.patch import ApiForpatch


class Fake(
    ApiForget,
    ApiForpost,
    ApiFordelete,
    ApiForpatch,
):
    pass
