import com.wordnik.swagger.online._
import com.wordnik.swagger.generator.model._
import com.wordnik.swagger.util.Json

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import scala.collection.JavaConverters._

@RunWith(classOf[JUnitRunner])
class GeneratorInputTest extends FlatSpec with Matchers {
  it should "write an object" in {
    val obj = new GeneratorInput()
    obj.setSwaggerUrl("http://petstore.swagger.io/v2/swagger.json")
  }
}