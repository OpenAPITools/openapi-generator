package Java

import io.swagger.codegen.languages.JavaClientCodegen
import io.swagger.models._
import io.swagger.models.properties._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FlatSpec, Matchers}

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
    enumVar.baseName should be("name")
    enumVar.datatype should be("String")
    enumVar.datatypeWithEnum should be("NameEnum")
    enumVar.name should be("name")
    enumVar.defaultValue should be("null")
    enumVar.baseType should be("String")
    enumVar.isEnum should equal(true)
  }
  
  it should "not override identical parent enums" in {
    
    val identicalEnumProperty = new StringProperty()
    identicalEnumProperty.setEnum(List("VALUE1", "VALUE2", "VALUE3").asJava)
    
    val subEnumProperty = new StringProperty()
    subEnumProperty.setEnum(List("SUB1", "SUB2", "SUB3").asJava)
    
    // Add one enum ptoperty to the parent
    val parentProperties = new java.util.HashMap[String, Property]()
    parentProperties.put("sharedThing", identicalEnumProperty)
    
    // Add TWO enums to the subType model; one of which is identical to the one in parent class
    val subProperties = new java.util.HashMap[String, Property]()
    subProperties.put("sharedThing", identicalEnumProperty)
    subProperties.put("unsharedThing", identicalEnumProperty)

    val parentModel = new ModelImpl();
    parentModel.setProperties(parentProperties);
    parentModel.name("parentModel");
        
    val subModel = new ModelImpl();
    subModel.setProperties(subProperties);
    subModel.name("subModel");
    
    val model = new ComposedModel()
      .parent(new RefModel(parentModel.getName()))
      .child(subModel)
      .interfaces(new java.util.ArrayList[RefModel]())
      
    val codegen = new JavaClientCodegen()
    val allModels = new java.util.HashMap[String, Model]()
    allModels.put(codegen.toModelName(parentModel.getName()), parentModel)
    allModels.put(codegen.toModelName(subModel.getName()), subModel)
    
    val cm = codegen.fromModel("sample", model, allModels)

    cm.name should be("sample")
    cm.classname should be("Sample")
    cm.parent should be("ParentModel")
    cm.imports.asScala should be(Set("ParentModel"))
    
    // Assert that only the unshared/uninherited enum remains
    cm.vars.size should be (1)
    val enumVar = cm.vars.get(0)
    enumVar.baseName should be("unsharedThing")
    enumVar.datatype should be("String")
    enumVar.datatypeWithEnum should be("UnsharedThingEnum")
    enumVar.isEnum should equal(true)
  }
}