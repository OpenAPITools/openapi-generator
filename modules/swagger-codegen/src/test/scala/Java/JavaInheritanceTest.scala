package Java

import scala.collection.JavaConverters._

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import io.swagger.codegen.languages.JavaClientCodegen
import io.swagger.models.ComposedModel
import io.swagger.models.ModelImpl
import io.swagger.models.RefModel
import io.swagger.models.properties.StringProperty

@RunWith(classOf[JUnitRunner])
class JavaInheritanceTest extends FlatSpec with Matchers {
  it should "convert a composed model" in {
    val model = new ComposedModel().parent(new RefModel("Base")).child(new ModelImpl().additionalProperties(new StringProperty()))

    val codegen = new JavaClientCodegen()
    val cm = codegen.fromModel("sample", model)

    cm.name should be("sample")
    cm.classname should be("Sample")
    cm.parent should be("Base")
    cm.imports.asScala should be (Set("Base"))
  }
}
