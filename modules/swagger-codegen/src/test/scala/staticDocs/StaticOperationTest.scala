package staticDocs

import io.swagger.codegen.languages.StaticDocCodegen
import io.swagger.util.Json
import io.swagger.models._
import io.swagger.models.properties._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import scala.collection.JavaConverters._

@RunWith(classOf[JUnitRunner])
class StaticOperationTest extends FlatSpec with Matchers {
  it should "convert a string parameter" in {
    val property = new StringProperty()

    val codegen = new StaticDocCodegen()
    val cp = codegen.fromProperty("property", property)

    cp.baseName should be ("property")
    cp.datatype should be ("String")
    cp.name should be ("property")
    cp.baseType should be ("string")
    cp.isNotContainer should equal (true)
  }

  it should "convert a complex parameter" in {
    val property = new RefProperty("Children")

    val codegen = new StaticDocCodegen()
    val cp = codegen.fromProperty("property", property)

    cp.baseName should be ("property")
    cp.complexType should be ("Children")
    cp.getter should be ("getProperty")
    cp.setter should be ("setProperty")
    cp.datatype should be ("Children")
    cp.name should be ("property")
    cp.defaultValue should be ("null")
    cp.baseType should be ("Children")
    cp.isNotContainer should equal (true)
  }

  it should "convert a complex list parameter" in {
    val property = new ArrayProperty().
      items(new RefProperty("Children"))

    val codegen = new StaticDocCodegen()
    val cp = codegen.fromProperty("property", property)

    cp.baseName should be ("property")
    cp.complexType should be ("Children")
    cp.getter should be ("getProperty")
    cp.setter should be ("setProperty")
    cp.datatype should be ("List")
    cp.name should be ("property")
    cp.baseType should be ("array")
    cp.containerType should be ("array")
    cp.isContainer should equal (true)

  }
}