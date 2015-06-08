import io.swagger.parser._
import org.junit.runner.RunWith
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SwaggerMigratorTest extends FlatSpec with Matchers {
  behavior of "SwaggerMigrator"

  it should "read a 1.2 spec" in {
    val loader = new SwaggerParser()
    val swagger = loader.read("src/test/resources/1_2/petstore-1.2/api-docs")
  }
}