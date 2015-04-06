package Java

import com.wordnik.swagger.codegen.languages.JavaClientCodegen
import com.wordnik.swagger.util.Json
import com.wordnik.swagger.models._
import com.wordnik.swagger.models.properties._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import scala.collection.JavaConverters._

@RunWith(classOf[JUnitRunner])
class JavaModelTest extends FlatSpec with Matchers {
  it should "convert a simple java model" in {
    val model = new ModelImpl()
      .description("a sample model")
      .property("id", new LongProperty())
      .property("name", new StringProperty())
      .property("createdAt", new DateTimeProperty())
      .required("id")
      .required("name")

    val codegen = new JavaClientCodegen()
    val cm = codegen.fromModel("sample", model)

    cm.name should be ("sample")
    cm.classname should be ("Sample")
    cm.description should be ("a sample model")
    cm.vars.size should be (3)

    val vars = cm.vars
    vars.get(0).baseName should be ("id")
    vars.get(0).getter should be ("getId")
    vars.get(0).setter should be ("setId")
    vars.get(0).datatype should be ("Long")
    vars.get(0).name should be ("id")
    vars.get(0).defaultValue should be ("null")
    vars.get(0).baseType should be ("Long")
    vars.get(0).hasMore should equal (true)
    vars.get(0).required should equal (true)
    vars.get(0).isNotContainer should equal (true)

    vars.get(1).baseName should be ("name")
    vars.get(1).getter should be ("getName")
    vars.get(1).setter should be ("setName")
    vars.get(1).datatype should be ("String")
    vars.get(1).name should be ("name")
    vars.get(1).defaultValue should be ("null")
    vars.get(1).baseType should be ("String")
    vars.get(1).hasMore should equal (true)
    vars.get(1).required should equal (true)
    vars.get(1).isNotContainer should equal (true)

    vars.get(2).baseName should be ("createdAt")
    vars.get(2).getter should be ("getCreatedAt")
    vars.get(2).setter should be ("setCreatedAt")
    vars.get(2).datatype should be ("Date")
    vars.get(2).name should be ("createdAt")
    vars.get(2).defaultValue should be ("null")
    vars.get(2).baseType should be ("Date")
    vars.get(2).hasMore should equal (null)
    vars.get(2).required should equal (null)
    vars.get(2).isNotContainer should equal (true)
  }

  it should "convert a model with list property" in {
    val model = new ModelImpl()
      .description("a sample model")
      .property("id", new LongProperty())
      .property("urls", new ArrayProperty()
        .items(new StringProperty()))
      .required("id")

    val codegen = new JavaClientCodegen()
    val cm = codegen.fromModel("sample", model)

    cm.name should be ("sample")
    cm.classname should be ("Sample")
    cm.description should be ("a sample model")
    cm.vars.size should be (2)

    val vars = cm.vars
    vars.get(1).baseName should be ("urls")
    vars.get(1).getter should be ("getUrls")
    vars.get(1).setter should be ("setUrls")
    vars.get(1).datatype should be ("List<String>")
    vars.get(1).name should be ("urls")
    vars.get(1).defaultValue should be ("new ArrayList<String>() ")
    vars.get(1).baseType should be ("List")
    vars.get(1).containerType should be ("array")
    vars.get(1).required should equal (null)
    vars.get(1).isContainer should equal (true)
  }

  it should "convert a model with a map property" in {
    val model = new ModelImpl()
      .description("a sample model")
      .property("translations", new MapProperty()
        .additionalProperties(new StringProperty()))
      .required("id")

    val codegen = new JavaClientCodegen()
    val cm = codegen.fromModel("sample", model)

    cm.name should be ("sample")
    cm.classname should be ("Sample")
    cm.description should be ("a sample model")
    cm.vars.size should be (1)

    val vars = cm.vars
    vars.get(0).baseName should be ("translations")
    vars.get(0).getter should be ("getTranslations")
    vars.get(0).setter should be ("setTranslations")
    vars.get(0).datatype should be ("Map<String, String>")
    vars.get(0).name should be ("translations")
    vars.get(0).defaultValue should be ("new HashMap<String, String>() ")
    vars.get(0).baseType should be ("Map")
    vars.get(0).containerType should be ("map")
    vars.get(0).required should equal (null)
    vars.get(0).isContainer should equal (true)
  }


  ignore should "convert a model with a map with complex list property" in {
    val model = new ModelImpl()
      .description("a sample model")
      .property("translations", new MapProperty()
        .additionalProperties(
          new ArrayProperty().items(new RefProperty("Pet")))
        )
      .required("id")

    val codegen = new JavaClientCodegen()
    val cm = codegen.fromModel("sample", model)

    cm.name should be ("sample")
    cm.classname should be ("Sample")
    cm.description should be ("a sample model")
    cm.vars.size should be (1)

    val vars = cm.vars
    vars.get(0).baseName should be ("translations")
    vars.get(0).getter should be ("getTranslations")
    vars.get(0).setter should be ("setTranslations")
    vars.get(0).datatype should be ("Map<String, List<Pet>>")
    vars.get(0).name should be ("translations")
    vars.get(0).defaultValue should be ("new HashMap<String, List<Pet>>() ")
    vars.get(0).baseType should be ("Map")
    vars.get(0).containerType should be ("map")
    vars.get(0).required should equal (null)
    vars.get(0).isContainer should equal (true)
  }

  it should "convert a model with complex properties" in {
    val model = new ModelImpl()
      .description("a sample model")
      .property("children", new RefProperty("#/definitions/Children"))

    val codegen = new JavaClientCodegen()
    val cm = codegen.fromModel("sample", model)

    cm.name should be ("sample")
    cm.classname should be ("Sample")
    cm.description should be ("a sample model")
    cm.vars.size should be (1)

    val vars = cm.vars
    vars.get(0).baseName should be ("children")
    vars.get(0).getter should be ("getChildren")
    vars.get(0).setter should be ("setChildren")
    vars.get(0).datatype should be ("Children")
    vars.get(0).name should be ("children")
    vars.get(0).defaultValue should be ("null")
    vars.get(0).baseType should be ("Children")
    vars.get(0).required should equal (null)
    vars.get(0).isNotContainer should equal (true)
  }

  it should "convert a model with complex list property" in {
    val model = new ModelImpl()
      .description("a sample model")
      .property("children", new ArrayProperty()
        .items(new RefProperty("#/definitions/Children")))

    val codegen = new JavaClientCodegen()
    val cm = codegen.fromModel("sample", model)

    cm.name should be ("sample")
    cm.classname should be ("Sample")
    cm.description should be ("a sample model")
    cm.vars.size should be (1)

    val vars = cm.vars
    vars.get(0).baseName should be ("children")
    vars.get(0).complexType should be ("Children")
    vars.get(0).getter should be ("getChildren")
    vars.get(0).setter should be ("setChildren")
    vars.get(0).datatype should be ("List<Children>")
    vars.get(0).name should be ("children")
    vars.get(0).defaultValue should be ("new ArrayList<Children>() ")
    vars.get(0).baseType should be ("List")
    vars.get(0).containerType should be ("array")
    vars.get(0).required should equal (null)
    vars.get(0).isContainer should equal (true)
  }

  it should "convert a model with complex map property" in {
    val model = new ModelImpl()
      .description("a sample model")
      .property("children", new MapProperty()
        .additionalProperties(new RefProperty("#/definitions/Children")))

    val codegen = new JavaClientCodegen()
    val cm = codegen.fromModel("sample", model)

    cm.name should be ("sample")
    cm.classname should be ("Sample")
    cm.description should be ("a sample model")
    cm.vars.size should be (1)
    (cm.imports.asScala.toSet & Set("Map", "List", "Children")).size should be (3)

    val vars = cm.vars
    vars.get(0).baseName should be ("children")
    vars.get(0).complexType should be ("Children")
    vars.get(0).getter should be ("getChildren")
    vars.get(0).setter should be ("setChildren")
    vars.get(0).datatype should be ("Map<String, Children>")
    vars.get(0).name should be ("children")
    vars.get(0).defaultValue should be ("new HashMap<String, Children>() ")
    vars.get(0).baseType should be ("Map")
    vars.get(0).containerType should be ("map")
    vars.get(0).required should equal (null)
    vars.get(0).isContainer should equal (true)
    vars.get(0).isNotContainer should be (null)
  }

  it should "convert an array model" in {
    val model = new ArrayModel()
      .description("an array model")
      .items(new RefProperty("#/definitions/Children"))
    val codegen = new JavaClientCodegen()
    val cm = codegen.fromModel("sample", model)

    cm.name should be ("sample")
    cm.classname should be ("Sample")
    cm.description should be ("an array model")
    cm.vars.size should be (0)
    cm.parent should be ("ArrayList<Children>")
    cm.imports.size should be (3)
    (cm.imports.asScala.toSet & Set("List", "ArrayList", "Children")).size should be (3)
  }

  it should "convert an map model" in {
    val model = new ModelImpl()
      .description("an map model")
      .additionalProperties(new RefProperty("#/definitions/Children"))

    val codegen = new JavaClientCodegen()
    val cm = codegen.fromModel("sample", model)

    cm.name should be ("sample")
    cm.classname should be ("Sample")
    cm.description should be ("an map model")
    cm.vars.size should be (0)
    cm.parent should be ("HashMap<String, Children>")
    cm.imports.size should be (3)
    (cm.imports.asScala.toSet & Set("Map", "HashMap", "Children")).size should be (3)
  }

  it should "convert a model with upper-case property names" in {
    val model = new ModelImpl()
      .description("a model with upper-case property names")
      .property("NAME", new StringProperty())
      .required("NAME")

    val codegen = new JavaClientCodegen()
    val cm = codegen.fromModel("sample", model)

    cm.name should be ("sample")
    cm.classname should be ("Sample")
    cm.vars.size should be (1)

    val vars = cm.vars
    vars.get(0).baseName should be ("NAME")
    vars.get(0).getter should be ("getNAME")
    vars.get(0).setter should be ("setNAME")
    vars.get(0).datatype should be ("String")
    vars.get(0).name should be ("NAME")
    vars.get(0).defaultValue should be ("null")
    vars.get(0).baseType should be ("String")
    vars.get(0).hasMore should equal (null)
    vars.get(0).required should equal (true)
    vars.get(0).isNotContainer should equal (true)
  }

  it should "convert a model with a 2nd char upper-case property names" in {
    val model = new ModelImpl()
      .description("a model with a 2nd char upper-case property names")
      .property("pId", new StringProperty())
      .required("pId")

    val codegen = new JavaClientCodegen()
    val cm = codegen.fromModel("sample", model)

    cm.name should be ("sample")
    cm.classname should be ("Sample")
    cm.vars.size should be (1)

    val vars = cm.vars
    vars.get(0).baseName should be ("pId")
    vars.get(0).getter should be ("getPId")
    vars.get(0).setter should be ("setPId")
    vars.get(0).datatype should be ("String")
    vars.get(0).name should be ("pId")
    vars.get(0).defaultValue should be ("null")
    vars.get(0).baseType should be ("String")
    vars.get(0).hasMore should equal (null)
    vars.get(0).required should equal (true)
    vars.get(0).isNotContainer should equal (true)
  }

  it should "convert hyphens per issue 503" in {
    val model = new ModelImpl()
      .description("a sample model")
      .property("created-at", new DateTimeProperty())

    val codegen = new JavaClientCodegen()
    val cm = codegen.fromModel("sample", model)
    val vars = cm.vars
    vars.get(0).baseName should be("created-at")
    vars.get(0).getter should be ("getCreatedAt")
    vars.get(0).setter should be ("setCreatedAt")
    vars.get(0).name should be ("createdAt")
  }

  it should "properly escape names per 567" in {
    val model = new ModelImpl()
      .description("a sample model")
      .property("created-at", new DateTimeProperty())

    val codegen = new JavaClientCodegen()
    val cm = codegen.fromModel("with.dots", model)
    val vars = cm.vars
    cm.classname should be ("WithDots")
  }
}
