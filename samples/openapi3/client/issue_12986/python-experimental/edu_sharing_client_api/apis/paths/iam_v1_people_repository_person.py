from edu_sharing_client_api.paths.iam_v1_people_repository_person.get import ApiForget
from edu_sharing_client_api.paths.iam_v1_people_repository_person.post import ApiForpost
from edu_sharing_client_api.paths.iam_v1_people_repository_person.delete import ApiFordelete


class IamV1PeopleRepositoryPerson(
    ApiForget,
    ApiForpost,
    ApiFordelete,
):
    pass
