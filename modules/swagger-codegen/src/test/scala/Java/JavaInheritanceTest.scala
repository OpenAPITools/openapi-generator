package Java

import scala.collection.JavaConverters._

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import com.wordnik.swagger.codegen.languages.JavaClientCodegen
import com.wordnik.swagger.models.ComposedModel
import com.wordnik.swagger.models.ModelImpl
import com.wordnik.swagger.models.RefModel
import com.wordnik.swagger.models.properties.StringProperty

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
