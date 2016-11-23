import connexion
from swagger_server.models.api_response import ApiResponse
from swagger_server.models.pet import Pet
from datetime import date, datetime
from typing import List, Dict
from six import iteritems
from ..util import deserialize_date, deserialize_datetime


def add_pet(body):
    """
    Add a new pet to the store
    
    :param body: Pet object that needs to be added to the store
    :type body: dict | bytes

    :rtype: None
    """
    if connexion.request.is_json:
        body = Pet.from_dict(connexion.request.get_json())
    return 'do some magic!'


def delete_pet(petId, api_key=None):
    """
    Deletes a pet
    
    :param petId: Pet id to delete
    :type petId: int
    :param api_key: 
    :type api_key: str

    :rtype: None
    """
    return 'do some magic!'


def find_pets_by_status(status):
    """
    Finds Pets by status
    Multiple status values can be provided with comma separated strings
    :param status: Status values that need to be considered for filter
    :type status: List[str]

    :rtype: List[Pet]
    """
    return 'do some magic!'


def find_pets_by_tags(tags):
    """
    Finds Pets by tags
    Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
    :param tags: Tags to filter by
    :type tags: List[str]

    :rtype: List[Pet]
    """
    return 'do some magic!'


def get_pet_by_id(petId):
    """
    Find pet by ID
    Returns a single pet
    :param petId: ID of pet to return
    :type petId: int

    :rtype: Pet
    """
    return 'do some magic!'


def update_pet(body):
    """
    Update an existing pet
    
    :param body: Pet object that needs to be added to the store
    :type body: dict | bytes

    :rtype: None
    """
    if connexion.request.is_json:
        body = Pet.from_dict(connexion.request.get_json())
    return 'do some magic!'


def update_pet_with_form(petId, name=None, status=None):
    """
    Updates a pet in the store with form data
    
    :param petId: ID of pet that needs to be updated
    :type petId: int
    :param name: Updated name of the pet
    :type name: str
    :param status: Updated status of the pet
    :type status: str

    :rtype: None
    """
    return 'do some magic!'


def upload_file(petId, additionalMetadata=None, file=None):
    """
    uploads an image
    
    :param petId: ID of pet to update
    :type petId: int
    :param additionalMetadata: Additional data to pass to server
    :type additionalMetadata: str
    :param file: file to upload
    :type file: werkzeug.datastructures.FileStorage

    :rtype: ApiResponse
    """
    return 'do some magic!'
