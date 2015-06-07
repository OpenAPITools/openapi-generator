import io.swagger.models._
import io.swagger.parser._

import io.swagger.util.Json

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import scala.collection.JavaConverters._

@RunWith(classOf[JUnitRunner])
class SwaggerMigratorTest extends FlatSpec with Matchers {
  behavior of "SwaggerMigrator"

  it should "read a 1.2 spec" in {
    val loader = new SwaggerParser()
    val swagger = loader.read("src/test/resources/1_2/petstore-1.2/api-docs")
    Json.prettyPrint(swagger)
  }
}