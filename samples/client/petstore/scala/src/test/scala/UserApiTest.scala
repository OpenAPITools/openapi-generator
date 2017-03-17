import io.swagger.client._
import io.swagger.client.api._
import io.swagger.client.model._
 
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

import scala.collection.mutable.{ ListBuffer, HashMap }
import scala.collection.JavaConverters._
import scala.beans.BeanProperty

@RunWith(classOf[JUnitRunner])
class UserApiTest extends FlatSpec with Matchers with BeforeAndAfterAll {
  behavior of "UserApi"
  val api = new UserApi
  api.apiInvoker.defaultHeaders += "api_key" -> "special-key"

  // preparation before running a test
  override def beforeAll() {
    val user = User(
      Some(11222),
      Some("scala-test-username"),
      Some("scala-test-first"),
      Some("scala-test-last"),
      Some("scala_test@fail.com"),
      Some("SCALATEST"),
      Some("408-867-5309"),
      Some(1))

    api.createUser(user)
  }

  // cleanup after running a test
  override def afterAll() {
    api.deleteUser("scala-test-username")
  }

  it should "fetch a user" in {
    api.getUserByName("scala-test-username") match {
      case Some(user) => {
        user.id should be(Some(11222))
        user.username should be(Some("scala-test-username"))
        user.password should be(Some("SCALATEST"))
        user.email should be(Some("scala_test@fail.com"))
        user.firstName should be(Some("scala-test-first"))
        user.lastName should be(Some("scala-test-last"))
        user.phone should be(Some("408-867-5309"))
        user.userStatus should be(Some(1))
      }
      case None =>
    }
  }

  it should "authenticate a user" in {
    api.loginUser("scala-test-username", "SCALATEST") match {
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

  it should "create 2 users" in {
    val userArray = (for (i <- (1 to 2)) yield {
      User(
        Some(2000 + i),
        Some("johnny-" + i),
        Some("Johnny"),
        Some("Rocket-" + i),
        Some("johnny-" + i + "@fail.com"),
        Some("XXXXXXXXXXX"),
        Some("408-867-5309"),
        Some(1))
    }).toList
    api.createUsersWithArrayInput(userArray)

    for (i <- (1 to 2)) {
      api.getUserByName("johnny-" + i) match {
        case Some(user) => {
          user.id should be(Some(2000 + i))
          user.email should be(Some("johnny-" + i + "@fail.com"))
        }
        case None => fail("didn't find user " + i)
      }
    }
  }

  it should "create 3 users" in {
    val userList = (for (i <- (1 to 3)) yield {
      User(
        Some(3000 + i),
        Some("fred-" + i),
        Some("Johnny"),
        Some("Rocket-" + i),
        Some("fred-" + i + "@fail.com"),
        Some("XXXXXXXXXXX"),
        Some("408-867-5309"),
        Some(1))
    }).toList
    api.createUsersWithListInput(userList)

    for (i <- (1 to 3)) {
      api.getUserByName("fred-" + i) match {
        case Some(user) => {
          user.id should be(Some(3000 + i))
          user.email should be(Some("fred-" + i + "@fail.com"))
        }
        case None => fail("didn't find user " + i)
      }
    }
  }

  it should "update a user" in {
    val user = User(
      Some(4000),
      Some("tony"),
      Some("Tony"),
      Some("Tiger"),
      Some("tony@fail.com"),
      Some("XXXXXXXXXXX"),
      Some("408-867-5309"),
      Some(1))

    api.createUser(user)

    api.getUserByName("tony") match {
      case Some(user) => {
        user.id should be(Some(4000))
        user.username should be(Some("tony"))
      }
      case None =>
    }

    val updatedUser = user.copy(email = Some("tony@succeed.com"))

    api.updateUser("tony", updatedUser)
    api.getUserByName("tony") match {
      case Some(user) => {
        user.email should be(Some("tony@succeed.com"))
      }
      case None =>
    }
  }
}
