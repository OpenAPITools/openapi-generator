# coding: utf-8

from typing import ClassVar, Dict, List, Tuple  # noqa: F401

from pydantic import Field, StrictBytes, StrictInt, StrictStr, field_validator
from typing import Any, List, Optional, Tuple, Union
from typing_extensions import Annotated
from openapi_server.models.api_response import ApiResponse
from openapi_server.models.pet import Pet
from openapi_server.security_api import get_token_petstore_auth, get_token_api_key

class BasePetApi:
    subclasses: ClassVar[Tuple] = ()

    def __init_subclass__(cls, **kwargs):
        super().__init_subclass__(**kwargs)
        BasePetApi.subclasses = BasePetApi.subclasses + (cls,)
    async def add_pet(
        self,
        pet: Annotated[Pet, Field(description="Pet object that needs to be added to the store")],
    ) -> Pet:
        """"""
        ...


    async def delete_pet(
        self,
        petId: Annotated[StrictInt, Field(description="Pet id to delete")],
        api_key: Optional[StrictStr],
    ) -> None:
        """"""
        ...


    async def find_pets_by_status(
        self,
        status: Annotated[List[StrictStr], Field(description="Status values that need to be considered for filter")],
    ) -> List[Pet]:
        """Multiple status values can be provided with comma separated strings"""
        ...


    async def find_pets_by_tags(
        self,
        tags: Annotated[List[StrictStr], Field(description="Tags to filter by")],
    ) -> List[Pet]:
        """Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing."""
        ...


    async def get_pet_by_id(
        self,
        petId: Annotated[StrictInt, Field(description="ID of pet to return")],
    ) -> Pet:
        """Returns a single pet"""
        ...


    async def update_pet(
        self,
        pet: Annotated[Pet, Field(description="Pet object that needs to be added to the store")],
    ) -> Pet:
        """"""
        ...


    async def update_pet_with_form(
        self,
        petId: Annotated[StrictInt, Field(description="ID of pet that needs to be updated")],
        name: Annotated[Optional[StrictStr], Field(description="Updated name of the pet")],
        status: Annotated[Optional[StrictStr], Field(description="Updated status of the pet")],
    ) -> None:
        """"""
        ...


    async def upload_file(
        self,
        petId: Annotated[StrictInt, Field(description="ID of pet to update")],
        additional_metadata: Annotated[Optional[StrictStr], Field(description="Additional data to pass to server")],
        file: Annotated[Optional[Union[StrictBytes, StrictStr, Tuple[StrictStr, StrictBytes]]], Field(description="file to upload")],
    ) -> ApiResponse:
        """"""
        ...
