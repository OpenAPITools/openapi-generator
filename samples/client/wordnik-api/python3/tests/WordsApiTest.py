#!/usr/bin/env python

import sys
import unittest
import urllib.request, urllib.error, urllib.parse
import json

from BaseApiTest import BaseApiTest

sys.path = ['./'] + sys.path
from wordnik import *


class WordsApiTest(BaseApiTest):

    def testSearchWords(self):
        res = self.wordsApi.searchWords('tree')
        assert res, 'null search result'
        assert res.searchResults[0].word == 'tree', 'word should be "tree"'
        assert res.totalResults != 0, 'should not have 0 results'

    def testGetWordOfTheDay(self):
        res = self.wordsApi.getWordOfTheDay()
        assert res, 'null wordOfTheDay result'

    def testReverseDictionary(self):
        res = self.wordsApi.reverseDictionary("hairy")
        assert res, 'null reverseDictionary result'
        assert res.totalResults != 0, 'should not have 0 results'
        assert len(res.results) != 0, 'should not have 0 results'

    def testGetRandomWords(self):
        res = self.wordsApi.getRandomWords()
        assert res, 'null getRandomWords result'
        assert len(res) == 10, 'should get 10 random words'

    def testGetRandomWords(self):
        res = self.wordsApi.getRandomWords()
        assert res, 'null getRandomWord result'

    def testGetRandomWord(self):
        res = self.wordsApi.getRandomWords()
        assert res, 'null getRandomWord result'


if __name__ == "__main__":
    unittest.main()
