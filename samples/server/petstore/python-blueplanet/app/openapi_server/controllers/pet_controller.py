import connexion
import six

from app.openapi_server.models.api_response import ApiResponse  # noqa: E501
from app.openapi_server.models.pet import Pet  # noqa: E501
from openapi_server import util


def add_pet(body):  # noqa: E501
    """Add a new pet to the store

     # noqa: E501

    :param body: Pet object that needs to be added to the store
    :type body: dict | bytes

    :rtype: None
    """
    if connexion.request.is_json:
        body = Pet.from_dict(connexion.request.get_json())  # noqa: E501
    return 'do some magic!'


def delete_pet(pet_id, api_key=None):  # noqa: E501
    """Deletes a pet

     # noqa: E501

    :param pet_id: Pet id to delete
    :type pet_id: int
    :param api_key: 
    :type api_key: str

    :rtype: None
    """
    return 'do some magic!'


def find_pets_by_status(status):  # noqa: E501
    """Finds Pets by status

    Multiple status values can be provided with comma separated strings # noqa: E501

    :param status: Status values that need to be considered for filter
    :type status: List[str]

    :rtype: List[Pet]
    """
    return 'do some magic!'


def find_pets_by_tags(tags):  # noqa: E501
    """Finds Pets by tags

    Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing. # noqa: E501

    :param tags: Tags to filter by
    :type tags: List[str]

    :rtype: List[Pet]
    """
    return 'do some magic!'


def get_pet_by_id(pet_id):  # noqa: E501
    """Find pet by ID

    Returns a single pet # noqa: E501

    :param pet_id: ID of pet to return
    :type pet_id: int

    :rtype: Pet
    """
    return 'do some magic!'


def update_pet(body):  # noqa: E501
    """Update an existing pet

     # noqa: E501

    :param body: Pet object that needs to be added to the store
    :type body: dict | bytes

    :rtype: None
    """
    if connexion.request.is_json:
        body = Pet.from_dict(connexion.request.get_json())  # noqa: E501
    return 'do some magic!'


def update_pet_with_form(pet_id, name=None, status=None):  # noqa: E501
    """Updates a pet in the store with form data

     # noqa: E501

    :param pet_id: ID of pet that needs to be updated
    :type pet_id: int
    :param name: Updated name of the pet
    :type name: str
    :param status: Updated status of the pet
    :type status: str

    :rtype: None
    """
    return 'do some magic!'


def upload_file(pet_id, additional_metadata=None, file=None):  # noqa: E501
    """uploads an image

     # noqa: E501

    :param pet_id: ID of pet to update
    :type pet_id: int
    :param additional_metadata: Additional data to pass to server
    :type additional_metadata: str
    :param file: file to upload
    :type file: str

    :rtype: ApiResponse
    """
    return 'do some magic!'
