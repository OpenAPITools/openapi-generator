#!/usr/bin/env python
"""Unit tests for Python Wordnik API client.

Requires you to set three environment varibales:
    API_KEY      your API key
    USER_NAME    the username of a user
    PASSWORD     the user's password

Run all tests:

    python BaseApiTest.py

"""

import sys
import os
import unittest

sys.path = ['./'] + sys.path
from wordnik import *


class BaseApiTest(unittest.TestCase):

    def setUp(self):
        self.apiUrl = 'http://api.wordnik.com/v4'
        self.apiKey = os.environ.get('API_KEY')
        self.username = os.environ.get('USER_NAME')
        self.password = os.environ.get('PASSWORD')

        client = swagger.ApiClient(self.apiKey, self.apiUrl)
        self.accountApi = AccountApi.AccountApi(client)
        self.wordApi = WordApi.WordApi(client)
        self.wordListApi = WordListApi.WordListApi(client)
        self.wordsApi = WordsApi.WordsApi(client)

if __name__ == "__main__":

    from AccountApiTest import AccountApiTest
    from WordApiTest import WordApiTest
    from WordListApiTest import WordListApiTest
    from WordsApiTest import WordsApiTest

    unittest.main()
