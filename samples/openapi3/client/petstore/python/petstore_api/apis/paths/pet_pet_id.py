from petstore_api.paths.pet_pet_id.get import ApiForget
from petstore_api.paths.pet_pet_id.post import ApiForpost
from petstore_api.paths.pet_pet_id.delete import ApiFordelete


class PetPetId(
    ApiForget,
    ApiForpost,
    ApiFordelete,
):
    pass
