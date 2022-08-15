from petstore_api.paths.pet_pet_id_3.get import ApiForget
from petstore_api.paths.pet_pet_id_3.post import ApiForpost
from petstore_api.paths.pet_pet_id_3.delete import ApiFordelete


class PetPetId(
    ApiForget,
    ApiForpost,
    ApiFordelete,
):
    pass
