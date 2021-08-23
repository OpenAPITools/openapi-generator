from uplink import (
    Consumer,
    get,
    Path,
    Query,
    Body,
    post,
    get,
    patch,
    put,
    delete,
    Header,
    returns,
    json,
)

from typing import Dict, List  # noqa: F401

from openapi_client.model.api_response import ApiResponse
from openapi_client.model.pet import Pet


class PetApi(Consumer):
    @json
    @post("/pet")
    def add_pet(self, *, body: Body(type=Pet)):
        """Add a new pet to the store"""

    @delete("/pet/{petId}")
    def delete_pet(self, *, petId: int, api_key: Header(None)):
        """Deletes a pet"""

    @returns.json
    @get("/pet/findByStatus")
    def find_pets_by_status(self, *, status: Query) -> List[Pet]:
        """Finds Pets by status"""

    @returns.json
    @get("/pet/findByTags")
    def find_pets_by_tags(self, *, tags: Query) -> List[Pet]:
        """Finds Pets by tags"""

    @returns.json
    @get("/pet/{petId}")
    def get_pet_by_id(self, *, petId: int) -> Pet:
        """Find pet by ID"""

    @json
    @put("/pet")
    def update_pet(self, *, body: Body(type=Pet)):
        """Update an existing pet"""

    @post("/pet/{petId}")
    def update_pet_with_form(self, *, petId: int, name: Form(None), status: Form(None)):
        """Updates a pet in the store with form data"""

    @returns.json
    @post("/pet/{petId}/uploadImage")
    def upload_file(self, *, petId: int, additional_metadata: Form(None), file: Form(None)) -> ApiResponse:
        """uploads an image"""

