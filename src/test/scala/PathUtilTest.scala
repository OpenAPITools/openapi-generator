import com.wordnik.swagger.codegen.BasicScalaGenerator

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class PathUtilTest extends FlatSpec with ShouldMatchers {
  val config = new BasicScalaGenerator

  behavior of "PathUtil"
  /*
   * We will take an api in the spec and create an API name from it
   */
  it should "convert an api name" in {
  	config.toApiName("fun") should be ("FunApi")
  }

  /*
   * We need to generate an API name from the resource path,
   * i.e. /foo will follow rules to become FooApi
  */
  it should "convert a path" in {
  	config.apiNameFromPath("/foo/bar/cats/dogs") should be ("FooApi")
  }
}