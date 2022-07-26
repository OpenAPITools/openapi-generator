from petstore_api.path.pet.put import ApiForput
from petstore_api.path.pet.post import ApiForpost

class Api(
    ApiForput,
    ApiForpost,
):
    pass
