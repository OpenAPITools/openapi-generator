from petstore_api.path.fake.get import ApiForget
from petstore_api.path.fake.post import ApiForpost
from petstore_api.path.fake.delete import ApiFordelete
from petstore_api.path.fake.patch import ApiForpatch

class Api(
    ApiForget,
    ApiForpost,
    ApiFordelete,
    ApiForpatch,
):
    pass
