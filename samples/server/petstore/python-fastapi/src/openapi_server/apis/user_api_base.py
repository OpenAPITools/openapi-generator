# coding: utf-8

from typing import ClassVar, Dict, List, Tuple  # noqa: F401

from openapi_server.models.user import User
from openapi_server.security_api import get_token_api_key

class BaseUserApi:
    subclasses: ClassVar[Tuple] = ()

    def __init_subclass__(cls, **kwargs):
        super().__init_subclass__(**kwargs)
        BaseUserApi.subclasses = BaseUserApi.subclasses + (cls,)
    async def create_user(
        self,
        user: User,
    ) -> None:
        """This can only be done by the logged in user."""
        ...


    async def create_users_with_array_input(
        self,
        user: List[User],
    ) -> None:
        """"""
        ...


    async def create_users_with_list_input(
        self,
        user: List[User],
    ) -> None:
        """"""
        ...


    async def delete_user(
        self,
        username: str,
    ) -> None:
        """This can only be done by the logged in user."""
        ...


    async def get_user_by_name(
        self,
        username: str,
    ) -> User:
        """"""
        ...


    async def login_user(
        self,
        username: str,
        password: str,
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
        username: str,
        user: User,
    ) -> None:
        """This can only be done by the logged in user."""
        ...
