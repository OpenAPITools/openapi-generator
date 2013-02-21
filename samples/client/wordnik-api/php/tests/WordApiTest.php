<?php

require_once 'BaseApiTest.php';

date_default_timezone_set('America/Los_Angeles');

class WordApiTest extends BaseApiTest {

  public function testWordApis() {
    $ch = curl_init("http://api.wordnik.com/v4/word.json");
    if (! $ch) {
    	die("No php curl handle");
    }
    curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);

    $data = curl_exec($ch);
    $doc = json_decode($data);

    $this->assertEquals(12, count($doc->apis));
  }

  public function testGetWord() {
    $res = $this->wordApi->getWord('cat');
    $this->assertEquals('cat', $res->word);
  }

  public function testGetWordWithSuggestions() {
    $res = $this->wordApi->getWord('cAt', $includeSuggestions=true);
    $this->assertEquals('cAt', $res->word);
  }

  public function testGetWordWithCanonicalForm() {
    $res = $this->wordApi->getWord('cAt', $useCanonical='true');
    $this->assertEquals('cat', $res->word);
  }

  public function testGetDefinitions() {
    $res = $this->wordApi->getDefinitions('cat');
    $this->assertEquals(15, count($res));
  }

  public function testGetDefinitionsWithSpacesInWord() {
    $res = $this->wordApi->getDefinitions('bon vivant');
    $this->assertEquals(1, count($res));
  }

  public function testGetExamples() {
    $res = $this->wordApi->getExamples('cat', $limit=5);
    $this->assertEquals(5, count($res->examples));
  }

  public function testGetTopExample() {
    $res = $this->wordApi->getTopExample('cat', $limit=5);
    $this->assertEquals('cat', $res->word);
  }

  public function testGetHyphenation() {
    $res = $this->wordApi->getHyphenation('catalog', $useCanonical=null, $sourceDictionary=null, $limit=1);
    $this->assertEquals(1, count($res));
  }

  public function testGetWordFrequency() {
    $res = $this->wordApi->getWordFrequency('catalog');
    $this->assertFalse($res->totalCount == 0);
  }

  public function testGetPhrases() {
    $res = $this->wordApi->getPhrases('money');
    $this->assertFalse(count($res) == 0);
  }

  public function testGetRelatedWords() {
    $res = $this->wordApi->getRelatedWords('cat');
    foreach ($res as $related) {
      $this->assertLessThan(11, count($related->words));
    }
  }

  public function testGetAudio() {
    $res = $this->wordApi->getAudio('cat', $useCanonical=True, $limit=2);
    $this->assertEquals(2, count($res));
  }

  public function testGetScrabbleScore() {
    $res = $this->wordApi->getScrabbleScore('quixotry');
    $this->assertEquals(27, $res->value);
  }

  public function testGetEtymologies() {
    $res = $this->wordApi->getEtymologies('butter');
    $this->assertFalse(strpos($res[0], 'of Scythian origin') === false);
  }

}
?>
