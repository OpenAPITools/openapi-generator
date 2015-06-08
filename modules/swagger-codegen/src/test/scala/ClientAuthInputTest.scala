import io.swagger.codegen._
import org.junit.runner.RunWith
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ClientAuthInputTest extends FlatSpec with Matchers {
  behavior of "ClientAuthInput"

  it should "read a file upload param from a 2.0 spec" in {
    val input = new ClientOptInput()

    val header = "api_key:special-key,api_key:hello,X-AUTHORIZATION:0e6c11d79a,Authorization:Basic 1jz0"
    input.setAuth(header)
    val authValues = input.getAuthorizationValues()
    authValues.size() should be(4)

    val a1 = authValues.get(0)
    a1.getKeyName should be("api_key")
    a1.getValue should be("special-key")
    a1.getType should be("header")

    val a2 = authValues.get(1)
    a2.getKeyName should be("api_key")
    a2.getValue should be("hello")
    a2.getType should be("header")

    val a3 = authValues.get(2)
    a3.getKeyName should be("X-AUTHORIZATION")
    a3.getValue should be("0e6c11d79a")
    a3.getType should be("header")

    val a4 = authValues.get(3)
    a4.getKeyName should be("Authorization")
    a4.getValue should be("Basic 1jz0")
    a4.getType should be("header")
  }
}