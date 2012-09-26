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
class AccountApiTest extends FlatSpec with ShouldMatchers with BaseApiTest {
  behavior of "AccountApi"
  val api = new AccountApi

  api.addHeader("api_key", API_KEY)

  val auth = api.authenticate(USER_NAME, PASSWORD).get

  it should "authenticate" in {
    api.authenticate(USER_NAME, PASSWORD) match {
      case Some(key) => {
        key.token should not be (null)
        key.userId should not be (0)
        key.userSignature should not be null
      }
      case None => fail("couldn't authenticate")
    }
  }

  it should "authenticate with post" in {
    api.authenticatePost(USER_NAME, PASSWORD) match {
      case Some(key) => {
        key.token should not be (null)
        key.userId should not be (0)
        key.userSignature should not be null
      }
      case None => fail("couldn't authenticate")
    }
  }

  it should "fetch word lists for the current user" in {
    api.getWordListsForLoggedInUser(auth.token, 0, 15) match {
      case Some(lists) => {
        lists.size should not be (0)
      }
      case None => fail("didn't fetch lists for user")
    }
  }

  it should "fetch api token status" in {
    api.getApiTokenStatus(API_KEY) match {
      case Some(status) => {
        status.valid should be(true)
        status.token should not be (null)
        status.remainingCalls should not be (0)
      }
      case None => fail("ooops")
    }
  }
  
  it should "get the logged in user" in {
    api.getLoggedInUser(auth.token) match {
      case Some(user) => {
        user.id should not be (0)
        user.username should be (USER_NAME)
        user.status should be (0)
        user.email should not be (null)
      }
      case None => fail("didn't get user")
    }
  }
}