import com.wordnik.swagger.core.util.JsonUtil
import com.wordnik.swagger.codegen.BasicGenerator
import com.wordnik.swagger.codegen.util._
import com.wordnik.swagger.core.{Documentation, DocumentationSchema}

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import scala.collection.mutable.HashMap
import scala.collection.JavaConverters._
import scala.reflect.BeanProperty

@RunWith(classOf[JUnitRunner])
class BasicGeneratorTest extends FlatSpec with ShouldMatchers {
  val json = ScalaJsonUtil.getJsonMapper

  class SampleGenerator extends BasicGenerator {
  	modelTemplateFiles += "model.mustache" -> ".test"
  	override def typeMapping = Map(
	    "string" -> "String",
	    "int" -> "Int",
	    "float" -> "Float",
	    "long" -> "Long",
	    "double" -> "Double",
	    "object" -> "Any")
  }

  behavior of "BasicGenerator"

  it should "get operations" in {
    val resourceListing = json.readValue(ResourceExtractor.extractListing("src/test/resources/petstore/resources.json", None), classOf[Documentation])
    val subDocs = ApiExtractor.extractApiDocs("src/test/resources/petstore", resourceListing.getApis.asScala.toList)
    val allModels = new HashMap[String, DocumentationSchema]()

    implicit val basePath = "http://localhost:8080/api"
    val generator = new SampleGenerator
    val ops = generator.extractOperations(subDocs, allModels)

  	allModels.size should be (5)
  	ops.size should be (16)

  	val operations = ops.map(op => (op._2, op._3)).toMap

  	(operations.keys.toSet & 
  	Set("/pet.{format}/findByTags", "/user.{format}/createWithArray", "/user.{format}/createWithList", 
  		"/store.{format}/order", "/user.{format}", "/pet.{format}/findByStatus", "/user.{format}/{username}",
  		"/user.{format}/logout", "/user.{format}/login", "/pet.{format}/{petId}", "/store.{format}/order/{orderId}", 
  		"/pet.{format}")).size should be (12)

  	// pick apart the /store/order api
  	val orderApi = operations("/store.{format}/order")

  	orderApi.httpMethod should be ("POST")
  	orderApi.summary should be ("Place an order for a pet")
  	orderApi.responseClass should be ("void")
  	orderApi.nickname should be ("placeOrder")
  	orderApi.getParameters.size should be (1)
  	orderApi.getErrorResponses.size should be (1)
  }

  it should "verify ops are grouped by path correctly" in {
    val resourceListing = json.readValue(ResourceExtractor.extractListing("src/test/resources/petstore/resources.json", None), classOf[Documentation])
    val subDocs = ApiExtractor.extractApiDocs("src/test/resources/petstore", resourceListing.getApis.asScala.toList)
    val allModels = new HashMap[String, DocumentationSchema]()

    implicit val basePath = "http://localhost:8080/api"
    val generator = new SampleGenerator
    val ops = generator.extractOperations(subDocs, allModels)
    val apiMap = generator.groupApisToFiles(ops)

    // verify all apis are there
    (apiMap.keys.map(m => m._2).toSet & Set("user", "pet", "store")).size should be (3)

    // inspect the store apis
    val orderApis = apiMap("http://petstore.swagger.wordnik.com/api","store").groupBy(_._1).toMap
    val orderOperations = orderApis("/store.{format}/order/{orderId}").map(m => m._2)

    // 2 operations
    orderOperations.size should be (2)
    (orderOperations.map(m => m.httpMethod).toSet & Set("GET", "DELETE")).size should be (2)
    (orderOperations.map(m => m.nickname).toSet & Set("getOrderById", "deleteOrder")).size should be (2)
  }

  it should "create a model map" in {
  	implicit val basePath = "http://localhost:8080/api"
    val generator = new SampleGenerator
    val model = getSampleModel

    val bundle = generator.prepareModelBundle(Map(model.id -> model)).head

    // inspect properties
    bundle("name") should be ("SampleObject")
    bundle("className") should be ("SampleObject")
    bundle("invokerPackage") should be (Some("com.wordnik.client.common"))
    bundle("package") should be (Some("com.wordnik.client.model"))

    // inspect models
    val modelList = bundle("models").asInstanceOf[List[(String, DocumentationSchema)]]
    modelList.size should be (1)
    modelList.head._1 should be ("SampleObject")
    modelList.head._2.getClass should be (classOf[DocumentationSchema])
  }

  it should "create a model file" in {
  	implicit val basePath = "http://localhost:8080/api"
    val generator = new SampleGenerator

    val model = getSampleModel
    val bundle = generator.prepareModelBundle(Map(model.id -> model))
    val modelFile = generator.bundleToSource(bundle, generator.modelTemplateFiles.toMap).head
    // modelFile._1 should be ("SampleObject.test")

    val fileContents = modelFile._2
    fileContents.indexOf("case class SampleObject") should not be (-1)
    fileContents.indexOf("longValue: Long") should not be (-1)
    fileContents.indexOf("intValue: Int") should not be (-1)
    fileContents.indexOf("doubleValue: Double") should not be (-1)
    fileContents.indexOf("stringValue: String") should not be (-1)
    fileContents.indexOf("floatValue: Float") should not be (-1)
  }

  def getSampleModel = {
    val model = new DocumentationSchema
    model.id = "SampleObject"
    model.name = "SampleObject"
    model.properties = {
      val list = new HashMap[String, DocumentationSchema]

      val stringProperty = new DocumentationSchema
      stringProperty.name = "stringValue"
      stringProperty.setType("string")
      list += "stringValue" -> stringProperty

      val intProperty = new DocumentationSchema
      intProperty.name = "intValue"
      intProperty.setType("int")
      list += "intValue" -> intProperty

      val longProperty = new DocumentationSchema
      longProperty.name = "longValue"
      longProperty.setType("long")
      list += "longValue" -> longProperty

      val floatProperty = new DocumentationSchema
      floatProperty.name = "floatValue"
      floatProperty.setType("float")
      list += "floatValue" -> floatProperty

      val doubleProperty = new DocumentationSchema
      doubleProperty.name = "doubleValue"
      doubleProperty.setType("double")
      list += "doubleValue" -> doubleProperty

      list.asJava
    }
    model
  }
}