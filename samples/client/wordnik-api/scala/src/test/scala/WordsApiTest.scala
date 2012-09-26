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
class WordsApiTest extends FlatSpec with ShouldMatchers with BaseApiTest {
  behavior of "WordsApi"
  val api = new WordsApi

  api.addHeader("api_key", API_KEY)

  it should "search words by path" in {
    api.searchWords("ca", null, null) match {
      case Some(words) => {
        words should not be (null)
        words.searchResults(0).word should be("ca")
        words.searchResults.size should be(11)
        words.totalResults should not be (0)
      }
      case None => fail("didn't find word cat")
    }
  }

  it should "get word of the day" in {
    api.getWordOfTheDay(null) match {
      case Some(wotd) => {
        wotd.word should not be (null)
      }
      case None => fail("didn't get wotd")
    }
  }

  it should "call the reverse dictionary" in {
    api.reverseDictionary("hairy", null, null, null, null, null, null, null, null) match {
      case Some(search) => {
        search.totalResults should not be (0)
        search.results.size should not be (0)
      }
      case None => fail("failed to get words")
    }
  }

  it should "get 10 random words" in {
    api.getRandomWords(null, null, null, null) match {
      case Some(words) => {
        words.size should be(10)
      }
      case None => fail("didn't get random words")
    }
  }

  it should "get one random words" in {
    api.getRandomWord(null, null) match {
      case Some(word) => {
        word should not be (null)
      }
      case None => fail("didn't get random word")
    }
  }
}