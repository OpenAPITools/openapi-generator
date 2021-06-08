# coding: utf-8

from fastapi.testclient import TestClient


from openapi_server.models.api_response import ApiResponse  # noqa: F401
from openapi_server.models.pet import Pet  # noqa: F401


def test_add_pet(client: TestClient):
    """Test case for add_pet

    Add a new pet to the store
    """
    pet = {"photo_urls":["photoUrls","photoUrls"],"name":"doggie","id":0,"category":{"name":"name","id":6},"tags":[{"name":"name","id":1},{"name":"name","id":1}],"status":"available"}

    headers = {
        "Authorization": "Bearer special-key",
    }
    response = client.request(
        "POST",
        "/pet",
        headers=headers,
        json=pet,
    )

    # uncomment below to assert the status code of the HTTP response
    #assert response.status_code == 200


def test_delete_pet(client: TestClient):
    """Test case for delete_pet

    Deletes a pet
    """

    headers = {
        "api_key": 'api_key_example',
        "Authorization": "Bearer special-key",
    }
    response = client.request(
        "DELETE",
        "/pet/{petId}".format(petId=56),
        headers=headers,
    )

    # uncomment below to assert the status code of the HTTP response
    #assert response.status_code == 200


def test_find_pets_by_status(client: TestClient):
    """Test case for find_pets_by_status

    Finds Pets by status
    """
    params = [("status", ['status_example'])]
    headers = {
        "Authorization": "Bearer special-key",
    }
    response = client.request(
        "GET",
        "/pet/findByStatus",
        headers=headers,
        params=params,
    )

    # uncomment below to assert the status code of the HTTP response
    #assert response.status_code == 200


def test_find_pets_by_tags(client: TestClient):
    """Test case for find_pets_by_tags

    Finds Pets by tags
    """
    params = [("tags", ['tags_example'])]
    headers = {
        "Authorization": "Bearer special-key",
    }
    response = client.request(
        "GET",
        "/pet/findByTags",
        headers=headers,
        params=params,
    )

    # uncomment below to assert the status code of the HTTP response
    #assert response.status_code == 200


def test_get_pet_by_id(client: TestClient):
    """Test case for get_pet_by_id

    Find pet by ID
    """

    headers = {
        "api_key": "special-key",
    }
    response = client.request(
        "GET",
        "/pet/{petId}".format(petId=56),
        headers=headers,
    )

    # uncomment below to assert the status code of the HTTP response
    #assert response.status_code == 200


def test_update_pet(client: TestClient):
    """Test case for update_pet

    Update an existing pet
    """
    pet = {"photo_urls":["photoUrls","photoUrls"],"name":"doggie","id":0,"category":{"name":"name","id":6},"tags":[{"name":"name","id":1},{"name":"name","id":1}],"status":"available"}

    headers = {
        "Authorization": "Bearer special-key",
    }
    response = client.request(
        "PUT",
        "/pet",
        headers=headers,
        json=pet,
    )

    # uncomment below to assert the status code of the HTTP response
    #assert response.status_code == 200


def test_update_pet_with_form(client: TestClient):
    """Test case for update_pet_with_form

    Updates a pet in the store with form data
    """

    headers = {
        "Authorization": "Bearer special-key",
    }
    data = {
        "name": 'name_example',
        "status": 'status_example'
    }
    response = client.request(
        "POST",
        "/pet/{petId}".format(petId=56),
        headers=headers,
        data=data,
    )

    # uncomment below to assert the status code of the HTTP response
    #assert response.status_code == 200


def test_upload_file(client: TestClient):
    """Test case for upload_file

    uploads an image
    """

    headers = {
        "Authorization": "Bearer special-key",
    }
    data = {
        "additional_metadata": 'additional_metadata_example',
        "file": '/path/to/file'
    }
    response = client.request(
        "POST",
        "/pet/{petId}/uploadImage".format(petId=56),
        headers=headers,
        data=data,
    )

    # uncomment below to assert the status code of the HTTP response
    #assert response.status_code == 200

