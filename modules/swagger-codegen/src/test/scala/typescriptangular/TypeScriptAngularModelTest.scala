package typescriptangular

import io.swagger.codegen.languages.TypeScriptAngularClientCodegen
import io.swagger.models._
import io.swagger.models.properties._
import org.junit.runner.RunWith
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.junit.JUnitRunner

import scala.collection.JavaConverters._

@RunWith(classOf[JUnitRunner])
class TypeScriptAngularModelTest extends FlatSpec with Matchers {

  it should "convert a simple TypeScript Angular model" in {
    val model = new ModelImpl()
      .description("a sample model")
      .property("id", new LongProperty())
      .property("name", new StringProperty())
      .property("createdAt", new DateTimeProperty())
      .required("id")
      .required("name")

    val codegen = new TypeScriptAngularClientCodegen()
    val cm = codegen.fromModel("sample", model)

    cm.name should be("sample")
    cm.classname should be("Sample")
    cm.description should be("a sample model")
    cm.vars.size should be(3)

    val vars = cm.vars
    vars.get(0).baseName should be("id")
    vars.get(0).datatype should be("number")
    vars.get(0).name should be("id")
    vars.get(0).defaultValue should be("null")
    vars.get(0).baseType should be("number")
    vars.get(0).hasMore should equal(true)
    vars.get(0).required should equal(true)
    vars.get(0).isNotContainer should equal(true)

    vars.get(1).baseName should be("name")
    vars.get(1).datatype should be("string")
    vars.get(1).name should be("name")
    vars.get(1).defaultValue should be("null")
    vars.get(1).baseType should be("string")
    vars.get(1).hasMore should equal(true)
    vars.get(1).required should equal(true)
    vars.get(1).isNotContainer should equal(true)

    vars.get(2).baseName should be("createdAt")
    vars.get(2).complexType should be("DateTime")
    vars.get(2).datatype should be("DateTime")
    vars.get(2).name should be("createdAt")
    vars.get(2).defaultValue should be("null")
    vars.get(2).hasMore should equal(null)
    vars.get(2).required should equal(null)
    vars.get(2).isNotContainer should equal(true)
  }

  it should "convert a model with list property" in {
    val model = new ModelImpl()
      .description("a sample model")
      .property("id", new LongProperty())
      .property("urls", new ArrayProperty()
      .items(new StringProperty()))
      .required("id")

    val codegen = new TypeScriptAngularClientCodegen()
    val cm = codegen.fromModel("sample", model)

    cm.name should be("sample")
    cm.classname should be("Sample")
    cm.description should be("a sample model")
    cm.vars.size should be(2)

    val vars = cm.vars
    vars.get(0).baseName should be("id")
    vars.get(0).datatype should be("number")
    vars.get(0).name should be("id")
    vars.get(0).defaultValue should be("null")
    vars.get(0).baseType should be("number")
    vars.get(0).hasMore should equal(true)
    vars.get(0).required should equal(true)
    vars.get(0).isNotContainer should equal(true)

    vars.get(1).baseName should be("urls")
    vars.get(1).datatype should be("Array<string>")
    vars.get(1).name should be("urls")
    vars.get(1).baseType should be("Array")
    vars.get(1).hasMore should be(null)
    vars.get(1).required should equal(null)
    vars.get(1).isContainer should equal(true)
  }

  it should "convert a model with complex property" in {
    val model = new ModelImpl()
      .description("a sample model")
      .property("children", new RefProperty("#/definitions/Children"))

    val codegen = new TypeScriptAngularClientCodegen()
    val cm = codegen.fromModel("sample", model)

    cm.name should be("sample")
    cm.classname should be("Sample")
    cm.description should be("a sample model")
    cm.vars.size should be(1)

    val vars = cm.vars
    vars.get(0).baseName should be("children")
    vars.get(0).datatype should be("Children")
    vars.get(0).name should be("children")
    vars.get(0).baseType should be("Children")
    vars.get(0).required should equal(null)
    vars.get(0).isNotContainer should equal(true)
  }

  it should "convert a model with complex list property" in {
    val model = new ModelImpl()
      .description("a sample model")
      .property("children", new ArrayProperty()
      .items(new RefProperty("#/definitions/Children")))

    val codegen = new TypeScriptAngularClientCodegen()
    val cm = codegen.fromModel("sample", model)

    cm.name should be("sample")
    cm.classname should be("Sample")
    cm.description should be("a sample model")
    cm.vars.size should be(1)

    val vars = cm.vars
    vars.get(0).baseName should be("children")
    vars.get(0).complexType should be("Children")
    vars.get(0).datatype should be("Array<Children>")
    vars.get(0).name should be("children")
    vars.get(0).baseType should be("Array")
    vars.get(0).required should equal(null)
    vars.get(0).isContainer should equal(true)
  }

  it should "convert an array model" in {
    val model = new ArrayModel()
      .description("an array model")
      .items(new RefProperty("#/definitions/Children"))
    val codegen = new TypeScriptAngularClientCodegen()
    val cm = codegen.fromModel("sample", model)

    cm.name should be("sample")
    cm.classname should be("Sample")
    cm.description should be("an array model")
    cm.vars.size should be(0)
  }

  it should "convert an map model" in {
    val model = new ModelImpl()
      .description("an map model")
      .additionalProperties(new RefProperty("#/definitions/Children"))

    val codegen = new TypeScriptAngularClientCodegen()
    val cm = codegen.fromModel("sample", model)

    cm.name should be("sample")
    cm.classname should be("Sample")
    cm.description should be("an map model")
    cm.vars.size should be(0)
    cm.imports.size should be(1)
    (cm.imports.asScala.toSet & Set("Children")).size should be(1)
  }
}
