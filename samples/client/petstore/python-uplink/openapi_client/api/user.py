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

from openapi_client.model.user import User


class UserApi(Consumer):
    @post("/user")
    def create_user(self, *, body: Body(type=User)):
        """Create user"""

    @post("/user/createWithArray")
    def create_users_with_array_input(self, *, body: Body(type=List[User])):
        """Creates list of users with given input array"""

    @post("/user/createWithList")
    def create_users_with_list_input(self, *, body: Body(type=List[User])):
        """Creates list of users with given input array"""

    @delete("/user/{username}")
    def delete_user(self, *, username: str):
        """Delete user"""

    @returns.json
    @get("/user/{username}")
    def get_user_by_name(self, *, username: str) -> User:
        """Get user by user name"""

    @returns.json
    @get("/user/login")
    def login_user(self, *, username: Query, password: Query) -> str:
        """Logs user into the system"""

    @get("/user/logout")
    def logout_user(self):
        """Logs out current logged in user session"""

    @put("/user/{username}")
    def update_user(self, *, username: str, body: Body(type=User)):
        """Updated user"""

