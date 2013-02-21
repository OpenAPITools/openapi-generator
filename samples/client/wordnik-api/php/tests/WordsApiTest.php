<?php

require_once 'BaseApiTest.php';

date_default_timezone_set('America/Los_Angeles');

class WordsApiTest extends BaseApiTest {

  public function testSearchWords() {
    $res = $this->wordsApi->searchWords('tree');
    $this->assertEquals('tree', $res->searchResults[0]->word);
    $this->assertNotEquals(0, $res->totalResults);
  }

  public function testGetWordOfTheDay() {
    $res = $this->wordsApi->getWordOfTheDay();
    $this->assertNotEquals(null, $res);
  }

  public function testReverseDictionary() {
    $res = $this->wordsApi->reverseDictionary('hairy');
    $this->assertNotEquals(0, $res->totalResults);
    $this->assertNotEquals(0, count($res->results));
  }

  public function testGetRandomWords() {
    $res = $this->wordsApi->getRandomWords();
    $this->assertEquals(10, count($res));
  }

  public function testGetRandomWord() {
    $res = $this->wordsApi->getRandomWord();
    $this->assertNotEquals(null, $res);
  }

}
?>
