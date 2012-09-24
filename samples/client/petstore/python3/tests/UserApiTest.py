#!/usr/bin/env python
import sys
import unittest
import random
import urllib

from BaseApiTest import BaseApiTest

sys.path = ['./'] + sys.path
from petstore import *
from petstore.models import *


def randomString():
    return str(randomInt())


def randomInt():
    return random.randint(1000, 100000)


class UserApiTest(BaseApiTest):

    @classmethod
    def setUpClass(cls):
        alpahbet = list('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
        random.shuffle(alpahbet)
        cls.randomUsername1 = ''.join(alpahbet)
        random.shuffle(alpahbet)
        cls.randomUsername2 = ''.join(alpahbet)
        random.shuffle(alpahbet)
        cls.randomUsername3 = ''.join(alpahbet)
        random.shuffle(alpahbet)
        cls.randomUsername4 = ''.join(alpahbet)
        random.shuffle(alpahbet)
        cls.randomUsername5 = ''.join(alpahbet)

    def testCreateUsersWithArrayInput(self):

        user = User.User()

        user.id = randomInt()
        user.lastName = randomString()
        user.username = self.randomUsername1
        user.phone = randomString()
        user.email = randomString()
        user.userStatus = int(randomString())
        user.firstName = randomString()
        user.password = randomString()

        otherUser = User.User()

        otherUser.id = randomInt()
        otherUser.lastName = randomString()
        otherUser.username = self.randomUsername2
        otherUser.phone = randomString()
        otherUser.email = randomString()
        otherUser.userStatus = int(randomString())
        otherUser.firstName = randomString()
        otherUser.password = randomString()

        users = [user, otherUser]
        self.userApi.createUsersWithArrayInput(users)

        newUser = self.userApi.getUserByName(self.randomUsername1)

        assert newUser.id == user.id, 'id matches user'
        assert newUser.lastName == user.lastName, 'lastName matches user'
        assert newUser.username == user.username, 'username matches user'
        assert newUser.phone == user.phone, 'phone matches user'
        assert newUser.email == user.email, 'email matches user'
        assert newUser.userStatus == user.userStatus, 'status matches user'
        assert newUser.firstName == user.firstName, 'firstName matches user'
        assert newUser.password == user.password, 'password matches user'

        newUser = self.userApi.getUserByName(self.randomUsername2)

        assert newUser.id == otherUser.id, 'id matches user'
        assert newUser.lastName == otherUser.lastName, 'lastName matches user'
        assert newUser.username == otherUser.username, 'username matches user'
        assert newUser.phone == otherUser.phone, 'phone matches user'
        assert newUser.email == otherUser.email, 'email matches user'
        assert newUser.userStatus == otherUser.userStatus, 'status matches user'
        assert newUser.firstName == otherUser.firstName, 'firstName matches user'
        assert newUser.password == otherUser.password, 'password matches user'

    def testCreateUsersWithListInput(self):

        user = User.User()

        user.id = randomInt()
        user.lastName = randomString()
        user.username = self.randomUsername3
        user.phone = randomString()
        user.email = randomString()
        user.userStatus = int(randomString())
        user.firstName = randomString()
        user.password = randomString()

        otherUser = User.User()

        otherUser.id = randomInt()
        otherUser.lastName = randomString()
        otherUser.username = self.randomUsername4
        otherUser.phone = randomString()
        otherUser.email = randomString()
        otherUser.userStatus = int(randomString())
        otherUser.firstName = randomString()
        otherUser.password = randomString()

        users = [user, otherUser]
        self.userApi.createUsersWithListInput(users)

        newUser = self.userApi.getUserByName(self.randomUsername3)

        assert newUser.id == user.id, 'id matches user'
        assert newUser.lastName == user.lastName, 'lastName matches user'
        assert newUser.username == user.username, 'username matches user'
        assert newUser.phone == user.phone, 'phone matches user'
        assert newUser.email == user.email, 'email matches user'
        assert newUser.userStatus == user.userStatus, 'status matches user'
        assert newUser.firstName == user.firstName, 'firstName matches user'
        assert newUser.password == user.password, 'password matches user'

        newUser = self.userApi.getUserByName(self.randomUsername4)

        assert newUser.id == otherUser.id, 'id matches user'
        assert newUser.lastName == otherUser.lastName, 'lastName matches user'
        assert newUser.username == otherUser.username, 'username matches user'
        assert newUser.phone == otherUser.phone, 'phone matches user'
        assert newUser.email == otherUser.email, 'email matches user'
        assert newUser.userStatus == otherUser.userStatus, 'status matches user'
        assert newUser.firstName == otherUser.firstName, 'firstName matches user'
        assert newUser.password == otherUser.password, 'password matches user'

    def testCreateUser(self):

        user = User.User()

        user.id = randomInt()
        user.lastName = randomString()
        user.username = self.randomUsername5
        user.phone = randomString()
        user.email = randomString()
        user.userStatus = int(randomString())
        user.firstName = randomString()
        user.password = randomString()

        self.userApi.createUser(user)

        newUser = self.userApi.getUserByName(self.randomUsername5)

        assert newUser.id, user.id
        assert newUser.lastName, user.lastName
        assert newUser.username, user.username
        assert newUser.phone, user.phone
        assert newUser.email, user.email
        assert newUser.userStatus, user.userStatus
        assert newUser.firstName, user.firstName
        assert newUser.password, user.password

    def testUpdateUser(self):

        user = User.User()
        username = randomString()
        user.id = randomInt()
        user.lastName = randomString()
        user.username = username
        user.phone = randomString()
        user.email = randomString()
        user.userStatus = int(randomString())
        user.firstName = randomString()
        user.password = randomString()

        self.userApi.createUser(user)

        user = self.userApi.getUserByName(username)

        user.lastName = randomString()
        user.phone = randomString()
        user.email = randomString()
        user.userStatus = int(randomString())
        user.firstName = randomString()
        user.password = randomString()

        self.userApi.updateUser(username, user)

        updatedUser = self.userApi.getUserByName(username)

        assert updatedUser.lastName == user.lastName, 'should match lastName'
        assert updatedUser.username == user.username, 'should match username'
        assert updatedUser.phone == user.phone, 'should match phone'
        assert updatedUser.email == user.email, 'should match email'
        assert updatedUser.userStatus == user.userStatus, 'should match status'
        assert updatedUser.firstName == user.firstName, 'should match firstName'
        assert updatedUser.password == user.password, 'should match password'

    def testDeleteUser(self):

        user = User.User()
        username = randomString()
        user.username = username

        self.userApi.createUser(user)

        self.userApi.deleteUser(username)

        userGone = False

        try:
            self.userApi.getUserByName(username)
        except urllib.request.HTTPError:
            userGone = True

        assert userGone, 'user should be deleted'

    def testLoginUser(self):

        res = self.userApi.loginUser("anyusername", "anypassword")

        assert res[:23] == "logged in user session:", 'should get session'

    def testLogoutUser(self):
        # We just want to make sure there are no errors in this test.
        self.userApi.logoutUser()


if __name__ == "__main__":
    unittest.main()
