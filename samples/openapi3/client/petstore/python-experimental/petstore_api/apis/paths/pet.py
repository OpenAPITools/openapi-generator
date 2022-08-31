from petstore_api.paths.pet.put import ApiForput
from petstore_api.paths.pet.post import ApiForpost


class Pet(
    ApiForput,
    ApiForpost,
):
    pass
