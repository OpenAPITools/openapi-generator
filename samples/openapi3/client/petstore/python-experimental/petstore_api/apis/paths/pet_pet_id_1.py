from petstore_api.paths.pet_pet_id_1.get import ApiForget
from petstore_api.paths.pet_pet_id_1.post import ApiForpost
from petstore_api.paths.pet_pet_id_1.delete import ApiFordelete


class PetPetId(
    ApiForget,
    ApiForpost,
    ApiFordelete,
):
    pass
