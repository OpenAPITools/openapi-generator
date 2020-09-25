from typing import List, Dict
from aiohttp import web

from openapi_server.models.user import User
from openapi_server import util


async def create_user(request: web.Request, body) -> web.Response:
    """Create user

    This can only be done by the logged in user.

    :param body: Created user object
    :type body: dict | bytes

    """
    body = User.from_dict(body)
    return web.Response(status=200)


async def create_users_with_array_input(request: web.Request, body) -> web.Response:
    """Creates list of users with given input array

    

    :param body: List of user object
    :type body: list | bytes

    """
    body = [User.from_dict(d) for d in body]
    return web.Response(status=200)


async def create_users_with_list_input(request: web.Request, body) -> web.Response:
    """Creates list of users with given input array

    

    :param body: List of user object
    :type body: list | bytes

    """
    body = [User.from_dict(d) for d in body]
    return web.Response(status=200)


async def delete_user(request: web.Request, username) -> web.Response:
    """Delete user

    This can only be done by the logged in user.

    :param username: The name that needs to be deleted
    :type username: str

    """
    return web.Response(status=200)


async def get_user_by_name(request: web.Request, username) -> web.Response:
    """Get user by user name

    

    :param username: The name that needs to be fetched. Use user1 for testing.
    :type username: str

    """
    return web.Response(status=200)


async def login_user(request: web.Request, username, password) -> web.Response:
    """Logs user into the system

    

    :param username: The user name for login
    :type username: str
    :param password: The password for login in clear text
    :type password: str

    """
    return web.Response(status=200)


async def logout_user(request: web.Request, ) -> web.Response:
    """Logs out current logged in user session

    


    """
    return web.Response(status=200)


async def update_user(request: web.Request, username, body) -> web.Response:
    """Updated user

    This can only be done by the logged in user.

    :param username: name that need to be deleted
    :type username: str
    :param body: Updated user object
    :type body: dict | bytes

    """
    body = User.from_dict(body)
    return web.Response(status=200)
