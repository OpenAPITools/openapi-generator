import io.swagger.generator.model._
import org.junit.runner.RunWith
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class GeneratorInputTest extends FlatSpec with Matchers {
  it should "write an object" in {
    val obj = new GeneratorInput()
    obj.setSwaggerUrl("http://petstore.swagger.io/v2/swagger.json")
  }
}