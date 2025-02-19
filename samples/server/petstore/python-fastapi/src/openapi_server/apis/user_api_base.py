# coding: utf-8

from typing import ClassVar, Dict, List, Tuple  # noqa: F401

from pydantic import Field, StrictStr, field_validator
from typing import Any, List
from typing_extensions import Annotated
from openapi_server.models.user import User
from openapi_server.security_api import get_token_api_key

class BaseUserApi:
    subclasses: ClassVar[Tuple] = ()

    def __init_subclass__(cls, **kwargs):
        super().__init_subclass__(**kwargs)
        BaseUserApi.subclasses = BaseUserApi.subclasses + (cls,)
    async def create_user(
        self,
        user: Annotated[User, Field(description="Created user object")],
    ) -> None:
        """This can only be done by the logged in user."""
        ...


    async def create_users_with_array_input(
        self,
        user: Annotated[List[User], Field(description="List of user object")],
    ) -> None:
        """"""
        ...


    async def create_users_with_list_input(
        self,
        user: Annotated[List[User], Field(description="List of user object")],
    ) -> None:
        """"""
        ...


    async def delete_user(
        self,
        username: Annotated[StrictStr, Field(description="The name that needs to be deleted")],
    ) -> None:
        """This can only be done by the logged in user."""
        ...


    async def get_user_by_name(
        self,
        username: Annotated[StrictStr, Field(description="The name that needs to be fetched. Use user1 for testing.")],
    ) -> User:
        """"""
        ...


    async def login_user(
        self,
        username: Annotated[str, Field(strict=True, description="The user name for login")],
        password: Annotated[StrictStr, Field(description="The password for login in clear text")],
    ) -> str:
        """"""
        ...


    async def logout_user(
        self,
    ) -> None:
        """"""
        ...


    async def update_user(
        self,
        username: Annotated[StrictStr, Field(description="name that need to be deleted")],
        user: Annotated[User, Field(description="Updated user object")],
    ) -> None:
        """This can only be done by the logged in user."""
        ...
