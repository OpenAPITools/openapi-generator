# coding: utf-8

from __future__ import absolute_import
import unittest

from openapi_server.models.api_response import ApiResponse  # noqa: E501
from openapi_server.models.user import User
from openapi_server.test import BaseTestCase


class TestUserAttributeMap(BaseTestCase):
    """Model serialization has to respect attribute_map on the class"""

    def setUp(self) -> None:

        self.data = {
            'id': 8,
            'username': "john",
            'first_name': "doe",
            'last_name': "johm",
            'email': "example@example.com",
            'password': "123",
            'phone': "123",
            'user_status': 1
        }

        self.desired_data = {
            'id': 8,
            'username': "john",
            'firstName': "doe",
            'lastName': "johm",
            'email': "example@example.com",
            'password': "123",
            'phone': "123",
            'userStatus': 1
        }

    def test_user_to_dict_default(self):
        """
        By default `attribute_map` uses `True` value,
        attribute_map of the class is respected.
        """
        user = User(**self.data)
        dikt = user.to_dict()
        self.assertEqual(dikt, self.desired_data)

    def test_user_to_dict_false(self):
        """
        Passing `False` into `to_dict` method does not use
        `attribute_map` on the class.
        """
        user = User(**self.data)
        dikt = user.to_dict(attr_map=False)
        self.assertEqual(dikt, self.data)

    def test_user_from_dict_to_dict(self):
        """
        Creates new `User` instance by using `from_dict` method
        passing in data from existing user `to_dict` method.
        Instances are their `dict` data are equal.
        """
        user = User(**self.data)
        user2 = User.from_dict(user.to_dict())

        self.assertEqual(user.to_dict(), user2.to_dict())
        self.assertEqual(user, user2)


if __name__ == '__main__':
    unittest.main()
