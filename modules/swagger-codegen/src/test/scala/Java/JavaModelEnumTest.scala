package Java

import io.swagger.codegen.languages.JavaClientCodegen
import io.swagger.models._
import io.swagger.models.properties._
import org.junit.runner.RunWith
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.junit.JUnitRunner

import scala.collection.JavaConverters._

@RunWith(classOf[JUnitRunner])
class JavaModelEnumTest extends FlatSpec with Matchers {

  it should "convert a java model with an enum" in {
    val enumProperty = new StringProperty()
    enumProperty.setEnum(List("VALUE1", "VALUE2", "VALUE3").asJava)
    val model = new ModelImpl()
      .property("name", enumProperty)

    val codegen = new JavaClientCodegen()
    val cm = codegen.fromModel("sample", model)

    cm.vars.size should be(1)
    val enumVar = cm.vars.get(0)
    enumVar.baseName should be ("name")
    enumVar.datatype should be ("String")
    enumVar.datatypeWithEnum should be("NameEnum")
    enumVar.name should be("name")
    enumVar.defaultValue should be("null")
    enumVar.baseType should be("String")
    enumVar.isEnum should equal(true)
  }
}