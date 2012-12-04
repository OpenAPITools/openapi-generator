import com.wordnik.client.api._
import com.wordnik.client.model._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import scala.collection.mutable.{ ListBuffer, HashMap }
import scala.collection.JavaConversions._
import scala.reflect.BeanProperty

@RunWith(classOf[JUnitRunner])
class WordListApiTest extends FlatSpec with ShouldMatchers with BaseApiTest {
  behavior of "WordListApi"
  val api = new WordListApi

  api.addHeader("api_key", API_KEY)

  val accountApi = new AccountApi
  val auth = accountApi.authenticate(USER_NAME, PASSWORD).get
  val existingList = accountApi.getWordListsForLoggedInUser(auth.token, 0, 1).get.head

  val wordListsApi = new WordListsApi

  val wordList = WordList(
      0,    //  id
      null, //  permalink
      "my test list", // name
      new java.util.Date, //created at
      null,   // updated at
      null,   //  lastActivityAt
      null,   //  username
      0,      //  user id
      "some words I want to play with", // description
      0,      //  words in list
      "PUBLIC") //  type

  var sampleList = wordListsApi.createWordList(wordList, auth.token) match {
    case Some(wl) => wl
    case None => throw new Exception("can't create test list to run tests with")
  }

  it should "get a word list by permalink" in {
    api.getWordListByPermalink(existingList.permalink, auth.token) match {
      case Some(list) => {
        list should not be (null)
      }
      case None => fail("didn't get existing word list")
    }
  }

  it should "update a word list" in {
    val description = "list updated at " + new java.util.Date

    val updatedList = existingList.copy(description = description)
    api.updateWordList(updatedList.permalink, updatedList, auth.token)

    api.getWordListByPermalink(updatedList.permalink, auth.token) match {
      case Some(list) => list.description should be(description)
      case None => fail("didn't update word list")
    }
  }

  it should "add words to a list" in {
    val wordsToAdd = new ListBuffer[StringValue]
    wordsToAdd += StringValue("delicious")
    wordsToAdd += StringValue("tasty")
    wordsToAdd += StringValue("scrumptious")

    api.addWordsToWordList(sampleList.permalink, wordsToAdd.toArray, auth.token)
  }

  it should "get word list words" in {
    api.getWordListWords(sampleList.permalink, auth.token) match {
      case Some(words) => {
        ((words.map(w => w.word)).toSet & Set("delicious", "tasty", "scrumptious")).size should be(3)
      }
      case None => fail("didn't get word list")
    }
  }

  it should "remove words from a list" in {
    val wordsToRemove = new ListBuffer[StringValue]
    wordsToRemove += StringValue("delicious")
    wordsToRemove += StringValue("tasty")

    api.deleteWordsFromWordList(sampleList.permalink, wordsToRemove.toArray, auth.token)
    api.getWordListWords(sampleList.permalink, auth.token).get.size should be(1)
  }

  it should "delete a word list" in {
    api.deleteWordList(sampleList.permalink, auth.token)
  }
}