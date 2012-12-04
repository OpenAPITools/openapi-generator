import com.wordnik.client.api._
import com.wordnik.client.model._

import com.wordnik.swagger.core._
import com.wordnik.swagger.core.util.JsonUtil

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import scala.collection.mutable.{ ListBuffer, HashMap }
import scala.collection.JavaConversions._
import scala.reflect.BeanProperty

import scala.io.Source

@RunWith(classOf[JUnitRunner])
class WordApiTest extends FlatSpec with ShouldMatchers with BaseApiTest {
  behavior of "WordApi"
  val api = new WordApi

  api.addHeader("api_key", API_KEY)

  it should "verify the word apis" in {
    val json = Source.fromURL("http://api.wordnik.com/v4/word.json").mkString
    val doc = JsonUtil.getJsonMapper.readValue(json, classOf[Documentation])
    assert(doc.getApis.size === 12)
  }

  it should "fetch a word" in {
    api.getWord("cat") match {
      case Some(word) => {
        word.word should be("cat")
        word should not be (null)
      }
      case None => fail("didn't find word cat")
    }
  }

  it should "fetch a word with suggestions" in {
    api.getWord("cAt", "false", "true") match {
      case Some(word) => {
        word.word should be("cAt")
        word.suggestions.size should be (1)
        word.suggestions(0) should be ("cat")
        word should not be (null)
      }
      case None => fail("didn't find word cAt")
    }
  }

  it should "fetch a word with canonical form" in {
    api.getWord("cAt", "true") match {
      case Some(word) => {
        word.word should be("cat")
        word should not be (null)
      }
      case None => fail("didn't find word cat")
    }
  }

  it should "fetch definitions for a word" in {
    api.getDefinitions("cat", null, null, 10) match {
      case Some(definitions) => {
        definitions.size should be(10)
      }
      case None => fail("didn't find definitions for cat")
    }
  }

  it should "fetch examples for a word" in {
    api.getExamples("cat", "false", "false", 0, 5) match {
      case Some(examples) => {
        examples.examples.size should be(5)
      }
      case None => fail("didn't find examples for cat")
    }
  }

  it should "fetch a top example for a word" in {
    api.getTopExample("cat") match {
      case Some(example) => {
        example.word should be("cat")
      }
      case None => fail("didn't find examples for cat")
    }
  }

  it should "get text pronunciations for a word" in {
    api.getTextPronunciations("cat", null, null, null, 2) match {
      case Some(prons) => {
        prons.size should be(2)
      }
      case None => fail("didn't find prons for cat")
    }
  }

  it should "get hyphenation for a word" in {
    api.getHyphenation("catalog", null, null) match {
      case Some(hyphenation) => {
        hyphenation.size should be(3)
      }
      case None => fail("didn't find hyphenation for catalog")
    }
  }

  it should "get word frequency for a word" in {
    api.getWordFrequency("cat") match {
      case Some(frequency) => {
        frequency.totalCount should not be (0)
      }
      case None => fail("didn't find frequency for cat")
    }
  }

  it should "get word phrases for a word" in {
    api.getPhrases("money", 10, 0) match {
      case Some(phrases) => {
        phrases.size should not be (0)
      }
      case None => fail("didn't find phrases for money")
    }
  }

  it should "get related words" in {
    api.getRelatedWords("cat", null, null) match {
      case Some(relateds) => {
        var count = 0
        relateds.foreach(related =>{
          related.words.size should (be <= 10)
        })
      }
      case None => fail("didn't find related words")
    }
  }

  it should "get audio for a word" in {
    api.getAudio("cat", "true", 2) match {
      case Some(audio) => {
        audio.size should be(2)
      }
      case None => fail("didn't find audio")
    }
  }
}
