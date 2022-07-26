from petstore_api.path.pet_pet_id.get import ApiForget
from petstore_api.path.pet_pet_id.post import ApiForpost
from petstore_api.path.pet_pet_id.delete import ApiFordelete

class Api(
    ApiForget,
    ApiForpost,
    ApiFordelete,
):
    pass
