<?php

require_once 'BaseApiTest.php';

date_default_timezone_set('America/Los_Angeles');

class WordListsApiTest extends BaseApiTest {

  public function setUp() {
    parent::setUp();
    $this->authToken = $this->accountApi->authenticate($this->username,
                                                      $this->password)->token;
  }


  public function testCreateWordList() {
    $wordList = new WordList();
    $wordList->name = "my test list";
    $wordList->type = "PUBLIC";
    $wordList->description = "some words I want to play with";

    $res = $this->wordListsApi->createWordList($body=$wordList,
                                               $this->authToken);
    $this->assertEquals($wordList->description, $res->description);

    $wordsToAdd = array();
    $word1 = new StringValue();
    $word1->word = "foo";
    $wordsToAdd[] = $word1;

    $this->wordListApi->addWordsToWordList($res->permalink,
                                           $body=$wordsToAdd,
                                           $this->authToken);



  }

}
?>
