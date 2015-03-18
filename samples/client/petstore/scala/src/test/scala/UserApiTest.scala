import io.swagger.client._
import io.swagger.client.api._
import io.swagger.client.model._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

import scala.collection.mutable.{ ListBuffer, HashMap }
import scala.collection.JavaConversions._
import scala.beans.BeanProperty

@RunWith(classOf[JUnitRunner])
class UserApiTest extends FlatSpec with Matchers {
  behavior of "UserApi"
  val api = new UserApi
  api.apiInvoker.defaultHeaders += "api_key" -> "special-key"

  it should "fetch a user" in {
    api.getUserByName("user1") match {
      case Some(user) => {
        user.id should be(1)
        user.username should be("user1")
        user.password should be("XXXXXXXXXXX")
        user.email should be("email1@test.com")
        user.firstName should be("first name 1")
        user.lastName should be("last name 1")
        user.phone should be("123-456-7890")
        user.userStatus should be(1)
      }
      case None =>
    }
  }
  
  it should "authenticate a user" in {
    api.loginUser("user1", "XXXXXXXXXXX") match {
      case Some(status) => status.startsWith("logged in user session") match {
        case true => // success!
        case _ => fail("didn't get expected message " + status)
      }
      case None => fail("not able to login")
    }
  }

  it should "log out a user" in {
    api.logoutUser
  }

  it should "create a user" in {
    val user = User(
      1002,
      "johnny",
      "Johnny",
      "Rocket",
      "johnny@fail.com",
      "XXXXXXXXXXX",
      "408-867-5309",
      1)

    api.createUser(user)

    api.getUserByName("johnny") match {
      case Some(user) => {
        user.id should be (1002)
        user.username should be ("johnny")
      }
      case None =>
    }
  }

  it should "create 2 users" in {
    val userArray = (for (i <- (1 to 2)) yield {
      User(
        2000 + i,
        "johnny-" + i,
        "Johnny",
        "Rocket-" + i,
        "johnny-" + i + "@fail.com",
        "XXXXXXXXXXX",
        "408-867-5309",
        1)
    }).toList
    api.createUsersWithArrayInput(userArray)
    
    for (i <- (1 to 2)) {
      api.getUserByName("johnny-" + i) match {
        case Some(user) => {
          user.id should be (2000 + i)
          user.email should be ("johnny-" + i + "@fail.com")
        }
        case None => fail("didn't find user " + i)
      }
    }
  }
  
  it should "create 3 users" in {
    val userList = (for (i <- (1 to 3)) yield {
      User(
        3000 + i,
        "fred-" + i,
        "Johnny",
        "Rocket-" + i,
        "fred-" + i + "@fail.com",
        "XXXXXXXXXXX",
        "408-867-5309",
        1)
    }).toList
    api.createUsersWithListInput(userList)

    for (i <- (1 to 3)) {
      api.getUserByName("fred-" + i) match {
        case Some(user) => {
          user.id should be (3000 + i)
          user.email should be ("fred-" + i + "@fail.com")
        }
        case None => fail("didn't find user " + i)
      }
    }
  }

  it should "update a user" in {
    val user = User(
      4000,
      "tony",
      "Tony",
      "Tiger",
      "tony@fail.com",
      "XXXXXXXXXXX",
      "408-867-5309",
      1)

    api.createUser(user)

    api.getUserByName("tony") match {
      case Some(user) => {
        user.id should be (4000)
        user.username should be ("tony")
      }
      case None =>
    }

    val updatedUser = user.copy(email="tony@succeed.com")

    api.updateUser("tony", updatedUser)
    api.getUserByName("tony") match {
      case Some(user) => {
        user.email should be ("tony@succeed.com")
      }
      case None =>
    }
  }
}