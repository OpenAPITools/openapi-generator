package objc

import com.wordnik.swagger.codegen.languages.ObjcClientCodegen
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
class ObjcModelTest extends FlatSpec with Matchers {

  it should "convert a simple java model" in {
    val model = new ModelImpl()
      .description("a sample model")
      .property("id", new LongProperty())
      .property("name", new StringProperty())
      .property("createdAt", new DateTimeProperty())
      .required("id")
      .required("name")

    val codegen = new ObjcClientCodegen()
    val cm = codegen.fromModel("sample", model)

    cm.name should be ("sample")
    cm.classname should be ("SWGSample")
    cm.description should be ("a sample model")
    cm.vars.size should be (3)

    val vars = cm.vars
    vars.get(0).baseName should be ("id")
    vars.get(0).datatype should be ("NSNumber*")
    vars.get(0).name should be ("_id")
    vars.get(0).defaultValue should be (null)
    vars.get(0).baseType should be ("NSNumber")
    vars.get(0).hasMore should equal (true)
    vars.get(0).required should equal (true)
    vars.get(0).isPrimitiveType should equal (true)
    vars.get(0).isNotContainer should equal (true)

    vars.get(1).baseName should be ("name")
    vars.get(1).datatype should be ("NSString*")
    vars.get(1).name should be ("name")
    vars.get(1).defaultValue should be (null)
    vars.get(1).baseType should be ("NSString")
    vars.get(1).hasMore should equal (true)
    vars.get(1).required should equal (true)
    vars.get(0).isPrimitiveType should equal (true)
    vars.get(1).isNotContainer should equal (true)

    vars.get(2).baseName should be ("createdAt")
    vars.get(2).datatype should be ("NSDate*")
    vars.get(2).name should be ("createdAt")
    vars.get(2).defaultValue should be (null)
    vars.get(2).baseType should be ("NSDate")
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

    val codegen = new ObjcClientCodegen()
    val cm = codegen.fromModel("sample", model)

    cm.name should be ("sample")
    cm.classname should be ("SWGSample")
    cm.description should be ("a sample model")
    cm.vars.size should be (2)

    val vars = cm.vars
    vars.get(0).baseName should be ("id")
    vars.get(0).datatype should be ("NSNumber*")
    vars.get(0).name should be ("_id")
    vars.get(0).defaultValue should be (null)
    vars.get(0).baseType should be ("NSNumber")
    vars.get(0).hasMore should equal (true)
    vars.get(0).required should equal (true)
    vars.get(0).isPrimitiveType should equal (true)
    vars.get(0).isNotContainer should equal (true)

    vars.get(1).baseName should be ("urls")
    vars.get(1).datatype should be ("NSArray*")
    vars.get(1).name should be ("urls")
    // vars.get(1).defaultValue should be ("null")
    vars.get(1).baseType should be ("NSArray")
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

    val codegen = new ObjcClientCodegen()
    val cm = codegen.fromModel("sample", model)

    cm.name should be ("sample")
    cm.classname should be ("SWGSample")
    cm.description should be ("a sample model")
    cm.vars.size should be (1)

    val vars = cm.vars
    vars.get(0).baseName should be ("translations")
    vars.get(0).datatype should be ("NSDictionary*")
    vars.get(0).name should be ("translations")
    vars.get(0).baseType should be ("NSDictionary")
    vars.get(0).containerType should be ("map")
    vars.get(0).required should equal (null)
    vars.get(0).isContainer should equal (true)
    vars.get(0).isPrimitiveType should equal (true)
  }

  it should "convert a model with complex property" in {
    val model = new ModelImpl()
      .description("a sample model")
      .property("children", new RefProperty("#/definitions/Children"))

    val codegen = new ObjcClientCodegen()
    val cm = codegen.fromModel("sample", model)

    cm.name should be ("sample")
    cm.classname should be ("SWGSample")
    cm.description should be ("a sample model")
    cm.vars.size should be (1)

    val vars = cm.vars
    vars.get(0).baseName should be ("children")
    vars.get(0).datatype should be ("SWGChildren*")
    vars.get(0).name should be ("children")
    vars.get(0).baseType should be ("SWGChildren")
    vars.get(0).required should equal (null)
    vars.get(0).isNotContainer should equal (true)
  }

  it should "convert a model with complex list property" in {
    val model = new ModelImpl()
      .description("a sample model")
      .property("children", new ArrayProperty()
        .items(new RefProperty("#/definitions/Children")))

    val codegen = new ObjcClientCodegen()
    val cm = codegen.fromModel("sample", model)

    cm.name should be ("sample")
    cm.classname should be ("SWGSample")
    cm.description should be ("a sample model")
    cm.vars.size should be (1)

    val vars = cm.vars
    vars.get(0).baseName should be ("children")
    vars.get(0).complexType should be ("SWGChildren")
    vars.get(0).datatype should be ("NSArray<SWGChildren>*")
    vars.get(0).name should be ("children")
    vars.get(0).baseType should be ("NSArray")
    vars.get(0).containerType should be ("array")
    vars.get(0).required should equal (null)
    vars.get(0).isContainer should equal (true)
  }

  it should "convert a model with complex map property" in {
    val model = new ModelImpl()
      .description("a sample model")
      .property("children", new MapProperty()
        .additionalProperties(new RefProperty("#/definitions/Children")))

    val codegen = new ObjcClientCodegen()
    val cm = codegen.fromModel("sample", model)

    cm.name should be ("sample")
    cm.classname should be ("SWGSample")
    cm.description should be ("a sample model")
    cm.vars.size should be (1)
    (cm.imports.asScala.toSet & Set("SWGChildren")).size should be (1)

    val vars = cm.vars
    vars.get(0).baseName should be ("children")
    vars.get(0).complexType should be ("SWGChildren")
    vars.get(0).datatype should be ("NSDictionary*")
    vars.get(0).name should be ("children")
    vars.get(0).baseType should be ("NSDictionary")
    vars.get(0).containerType should be ("map")
    vars.get(0).required should equal (null)
    vars.get(0).isContainer should equal (true)
    vars.get(0).isNotContainer should be (null)
  }

  it should "convert an array model" in {
    val model = new ArrayModel()
      .description("an array model")
      .items(new RefProperty("#/definitions/Children"))
    val codegen = new ObjcClientCodegen()
    val cm = codegen.fromModel("sample", model)

    cm.name should be ("sample")
    cm.classname should be ("SWGSample")
    cm.description should be ("an array model")
    cm.vars.size should be (0)
    cm.parent should be ("NSMutableArray")
    cm.imports.size should be (3)
    (cm.imports.asScala.toSet & Set("SWGChildren", "NSArray", "NSMutableArray")).size should be (3)
  }

  it should "convert an map model" in {
    val model = new ModelImpl()
      .description("an map model")
      .additionalProperties(new RefProperty("#/definitions/Children"))

    val codegen = new ObjcClientCodegen()
    val cm = codegen.fromModel("sample", model)

    cm.name should be ("sample")
    cm.classname should be ("SWGSample")
    cm.description should be ("an map model")
    cm.vars.size should be (0)
    cm.parent should be ("NSMutableDictionary")
    cm.imports.size should be (3)
    (cm.imports.asScala.toSet & Set("SWGChildren", "NSDictionary", "NSMutableDictionary")).size should be (3)
  }

  it should "create proper imports per #316" in {
    val model = new SwaggerParser().read("src/test/resources/2_0/postBodyTest.json")
    val codegen = new ObjcClientCodegen()

    val animalPaths = model.getPaths()
    val animalOps = animalPaths.get("/animals")
    animalOps.getPost() should not be (null)
    val animalCo = codegen.fromOperation("/animals", "POST", animalOps.getPost(), model.getDefinitions())
    animalCo.imports.size should be (1)
    animalCo.imports.contains("SWGAnimal") should equal (true)

    val insectPaths = model.getPaths()
    val insectOps = insectPaths.get("/insects")
    insectOps.getPost() should not be (null)
    val insectCo = codegen.fromOperation("/insects", "POST", insectOps.getPost(), model.getDefinitions())
    insectCo.imports.size should be (1)
    insectCo.imports.contains("SWGInsect") should equal (true)
  }
}
