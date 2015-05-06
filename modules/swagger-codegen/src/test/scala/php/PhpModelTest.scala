package php

import com.wordnik.swagger.codegen.languages.PhpClientCodegen
import com.wordnik.swagger.util.Json
import com.wordnik.swagger.models._
import com.wordnik.swagger.models.properties._

import io.swagger.parser.SwaggerParser

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import scala.collection.JavaConverters._

@RunWith(classOf[JUnitRunner])
class PhpModelTest extends FlatSpec with Matchers {

  it should "convert a simple php model" in {
    val model = new ModelImpl()
      .description("a sample model")
      .property("id", new LongProperty())
      .property("name", new StringProperty())
      .property("createdAt", new DateTimeProperty())
      .required("id")
      .required("name")

    val codegen = new PhpClientCodegen()
    val cm = codegen.fromModel("sample", model)

    cm.name should be ("sample")
    cm.classname should be ("Sample")
    cm.description should be ("a sample model")
    cm.vars.size should be (3)

    val vars = cm.vars
    vars.get(0).baseName should be ("id")
    vars.get(0).datatype should be ("int")
    vars.get(0).name should be ("id")
    vars.get(0).defaultValue should be ("null")
    vars.get(0).baseType should be ("int")
    vars.get(0).hasMore should equal (true)
    vars.get(0).required should equal (true)
    vars.get(0).isPrimitiveType should equal (true)
    vars.get(0).isNotContainer should equal (true)

    vars.get(1).baseName should be ("name")
    vars.get(1).datatype should be ("string")
    vars.get(1).name should be ("name")
    vars.get(1).defaultValue should be ("null")
    vars.get(1).baseType should be ("string")
    vars.get(1).hasMore should equal (true)
    vars.get(1).required should equal (true)
    vars.get(0).isPrimitiveType should equal (true)
    vars.get(1).isNotContainer should equal (true)

    vars.get(2).baseName should be ("createdAt")
    vars.get(2).complexType should be (null)
    vars.get(2).datatype should be ("DateTime")
    vars.get(2).name should be ("created_at")
    vars.get(2).defaultValue should be ("null")
    vars.get(2).baseType should be ("DateTime")
    vars.get(2).hasMore should equal (null)
    vars.get(2).required should equal (null)
    vars.get(2).isNotContainer should equal (true)

    cm.imports.size() should be (0)
  }

  it should "convert a model with list property" in {
    val model = new ModelImpl()
      .description("a sample model")
      .property("id", new LongProperty())
      .property("urls", new ArrayProperty()
        .items(new StringProperty()))
      .required("id")

    val codegen = new PhpClientCodegen()
    val cm = codegen.fromModel("sample", model)

    cm.name should be ("sample")
    cm.classname should be ("Sample")
    cm.description should be ("a sample model")
    cm.vars.size should be (2)

    val vars = cm.vars
    vars.get(0).baseName should be ("id")
    vars.get(0).datatype should be ("int")
    vars.get(0).name should be ("id")
    vars.get(0).defaultValue should be ("null")
    vars.get(0).baseType should be ("int")
    vars.get(0).hasMore should equal (true)
    vars.get(0).required should equal (true)
    vars.get(0).isPrimitiveType should equal (true)
    vars.get(0).isNotContainer should equal (true)

    vars.get(1).baseName should be ("urls")
    vars.get(1).datatype should be ("array[string]")
    vars.get(1).name should be ("urls")
    vars.get(1).baseType should be ("array")
    vars.get(1).hasMore should be (null)
    vars.get(1).containerType should equal ("array")
    vars.get(1).required should equal (null)
    vars.get(1).isPrimitiveType should equal (true)
    vars.get(1).isContainer should equal (true)
  }

  it should "convert a model with a map property" in {
    val model = new ModelImpl()
      .description("a sample model")
      .property("translations", new MapProperty()
        .additionalProperties(new StringProperty()))
      .required("id")

    val codegen = new PhpClientCodegen()
    val cm = codegen.fromModel("sample", model)

    cm.name should be ("sample")
    cm.classname should be ("Sample")
    cm.description should be ("a sample model")
    cm.vars.size should be (1)

    val vars = cm.vars
    vars.get(0).baseName should be ("translations")
    vars.get(0).datatype should be ("map[string,string]")
    vars.get(0).name should be ("translations")
    vars.get(0).baseType should be ("map")
    vars.get(0).containerType should be ("map")
    vars.get(0).required should equal (null)
    vars.get(0).isContainer should equal (true)
    vars.get(0).isPrimitiveType should equal (true)
  }

  it should "convert a model with complex property" in {
    val model = new ModelImpl()
      .description("a sample model")
      .property("children", new RefProperty("#/definitions/Children"))

    val codegen = new PhpClientCodegen()
    val cm = codegen.fromModel("sample", model)

    cm.name should be ("sample")
    cm.classname should be ("Sample")
    cm.description should be ("a sample model")
    cm.vars.size should be (1)

    val vars = cm.vars
    vars.get(0).baseName should be ("children")
    vars.get(0).datatype should be ("Children")
    vars.get(0).name should be ("children")
    vars.get(0).baseType should be ("Children")
    vars.get(0).required should equal (null)
    vars.get(0).isNotContainer should equal (true)
  }

  it should "convert a model with complex list property" in {
    val model = new ModelImpl()
      .description("a sample model")
      .property("children", new ArrayProperty()
        .items(new RefProperty("#/definitions/Children")))

    val codegen = new PhpClientCodegen()
    val cm = codegen.fromModel("sample", model)

    cm.name should be ("sample")
    cm.classname should be ("Sample")
    cm.description should be ("a sample model")
    cm.vars.size should be (1)

    val vars = cm.vars
    vars.get(0).baseName should be ("children")
    vars.get(0).complexType should be ("Children")
    vars.get(0).datatype should be ("array[Children]")
    vars.get(0).name should be ("children")
    vars.get(0).baseType should be ("array")
    vars.get(0).containerType should be ("array")
    vars.get(0).required should equal (null)
    vars.get(0).isContainer should equal (true)
  }

  it should "convert a model with complex map property" in {
    val model = new ModelImpl()
      .description("a sample model")
      .property("children", new MapProperty()
        .additionalProperties(new RefProperty("#/definitions/Children")))

    val codegen = new PhpClientCodegen()
    val cm = codegen.fromModel("sample", model)

    cm.name should be ("sample")
    cm.classname should be ("Sample")
    cm.description should be ("a sample model")
    cm.vars.size should be (1)
    (cm.imports.asScala.toSet & Set("Children")).size should be (1)

    val vars = cm.vars
    vars.get(0).baseName should be ("children")
    vars.get(0).complexType should be ("Children")
    vars.get(0).datatype should be ("map[string,Children]")
    vars.get(0).name should be ("children")
    vars.get(0).baseType should be ("map")
    vars.get(0).containerType should be ("map")
    vars.get(0).required should equal (null)
    vars.get(0).isContainer should equal (true)
    vars.get(0).isNotContainer should be (null)
  }

  it should "convert an array model" in {
    val model = new ArrayModel()
      .description("an array model")
      .items(new RefProperty("#/definitions/Children"))
    val codegen = new PhpClientCodegen()
    val cm = codegen.fromModel("sample", model)

    cm.name should be ("sample")
    cm.classname should be ("Sample")
    cm.description should be ("an array model")
    cm.vars.size should be (0)
    cm.imports.size should be (1)
    (cm.imports.asScala.toSet & Set("Children")).size should be (1)
  }

  it should "convert an map model" in {
    val model = new ModelImpl()
      .description("an map model")
      .additionalProperties(new RefProperty("#/definitions/Children"))

    val codegen = new PhpClientCodegen()
    val cm = codegen.fromModel("sample", model)

    cm.name should be ("sample")
    cm.classname should be ("Sample")
    cm.description should be ("an map model")
    cm.vars.size should be (0)
    cm.imports.size should be (2)
    (cm.imports.asScala.toSet & Set("Children")).size should be (1)
  }

  it should "create proper imports per #316" in {
    val model = new SwaggerParser()
      .read("src/test/resources/2_0/postBodyTest.json")
    val codegen = new PhpClientCodegen()

    val animalPaths = model.getPaths()
    val animalOps = animalPaths.get("/animals")
    animalOps.getPost() should not be (null)
    val animalCo = codegen.fromOperation("/animals", "POST", animalOps.getPost(), model.getDefinitions())
    animalCo.imports.size should be (1)
    animalCo.imports.contains("Animal") should equal (true)

    val insectPaths = model.getPaths()
    val insectOps = insectPaths.get("/insects")
    insectOps.getPost() should not be (null)
    val insectCo = codegen.fromOperation("/insects", "POST", insectOps.getPost(), model.getDefinitions())
    insectCo.imports.size should be (1)
    insectCo.imports.contains("Insect") should equal (true)
  }
}
