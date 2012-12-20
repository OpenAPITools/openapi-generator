#!/usr/bin/env python

import sys
import unittest
import urllib2
import json

from BaseApiTest import BaseApiTest

sys.path = ['./'] + sys.path
from wordnik import *


class AccountApiTest(BaseApiTest):

    def setUp(self):
        super(AccountApiTest, self).setUp()
        self.authToken = self.accountApi.authenticate(self.username,
                                                      self.password).token

    def testAuthenticate(self):
        res = self.accountApi.authenticate(self.username, self.password)
        assert res, 'null authenticate result'
        assert res.token, 'invalid authentication token'
        assert res.userId != 0, 'userId was 0'
        assert res.userSignature, 'invalid userSignature'

    def testAuthenticatePost(self):
        res = self.accountApi.authenticatePost(self.username, self.password)
        assert res, 'null authenticate result'
        assert res.token, 'invalid authentication token'
        assert res.userId != 0, 'userId was 0'
        assert res.userSignature, 'invalid userSignature'

    def testGetWordListsForLoggedInUser(self):
        res = self.accountApi.getWordListsForLoggedInUser(self.authToken)
        assert res, 'null getWordListsForLoggedInUser result'
        assert len(res) != 0, 'number of lists shouldn\'t be 0'

    def testGetApiTokenStatus(self):
        res = self.accountApi.getApiTokenStatus()
        assert res, 'null getApiTokenStatus result'
        assert res.valid, 'token status not valid'
        assert res.remainingCalls != 0, 'remainingCalls shouldn\'t be 0'

    def testGetLoggedInUser(self):
        res = self.accountApi.getLoggedInUser(self.authToken)
        assert res, 'null getLoggedInUser result'
        assert res.id != 0, 'if shouldn\'t be 0'
        assert res.username == self.username, 'username was incorrect'
        assert res.status == 0, 'user status should be 0'
        assert res.email, 'email shouldn\'t be null'


if __name__ == "__main__":
    unittest.main()
