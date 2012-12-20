#!/usr/bin/env python

import sys
import unittest
import urllib2
import json

from BaseApiTest import BaseApiTest

sys.path = ['./'] + sys.path
from wordnik import *


class WordApiTest(BaseApiTest):

    def testWordApis(self):
        response = urllib2.urlopen('http://api.wordnik.com/v4/word.json')
        doc = json.loads(response.read())
        assert len(doc['apis']) == 12, 'there should be 10 word apis'

    def testGetWord(self):
        res = self.wordApi.getWord('cat')
        assert res, 'null getWord result'
        assert res.word == 'cat', 'word should be "cat"'

    def testGetWordWithSuggestions(self):
        res = self.wordApi.getWord('cAt', includeSuggestions=True)
        assert res, 'null getWord result'
        assert res.word == 'cAt', 'word should be "cAt"'

    def testGetWordWithCanonicalForm(self):
        res = self.wordApi.getWord('cAt', useCanonical=True)
        assert res, 'null getWord result'
        assert res.word == 'cat', 'word should be "cAt"'

    def testGetDefinitions(self):
        res = self.wordApi.getDefinitions('cat', limit=10)
        assert res, 'null getDefinitions result'
        assert len(res) == 10, 'should have 10 definitions'

    def testGetDefinitionsWithSpacesInWord(self):
        res = self.wordApi.getDefinitions('bon vivant')
        assert res, 'null getDefinitions result'
        assert len(res) == 1, 'should have 1 definition'

    def testGetExamples(self):
        res = self.wordApi.getExamples('cat', limit=5)
        assert res, 'null getExamples result'
        assert len(res.examples) == 5, 'should have 5 definitions'

    def testGetTopExample(self):
        res = self.wordApi.getTopExample('cat')
        assert res, 'null getTopExample result'
        assert res.word == 'cat', 'word should be "cat"'

    def testGetHyphenation(self):
        res = self.wordApi.getHyphenation('catalog', limit=1)
        assert res, 'null getHyphenation result'
        assert len(res) == 1, 'hypenation length should be 1'

    def testGetWordFrequency(self):
        res = self.wordApi.getWordFrequency('cat')
        assert res, 'null getWordFrequency result'
        assert res.totalCount != 0, 'total count should not be 0'

    def testGetPhrases(self):
        res = self.wordApi.getPhrases('money')
        assert res, 'null getPhrases result'
        assert len(res) != 0, 'getPhrases length should not be 0'

    def testGetRelatedWords(self):
        res = self.wordApi.getRelatedWords('cat')
        assert res, 'null getRelatedWords result'
        for related in res:
            assert len(related.words) <= 10, 'should have <= 10 related words'

    def testGetAudio(self):
        res = self.wordApi.getAudio('cat', useCanonical=True, limit=2)
        assert res, 'null getAudio result'
        assert len(res) == 2, 'getAudio size should be 2'

    def testGetScrabbleScore(self):
        res = self.wordApi.getScrabbleScore('quixotry')
        assert res.value == 27, 'quixotry should have a Scrabble score of 27'

    def testGetEtymologies(self):
        res = self.wordApi.getEtymologies('butter')
        assert 'of Scythian origin' in res[0], 'etymology of "butter" should contain the phrase "of Scythian origin"'


if __name__ == "__main__":
    unittest.main()
