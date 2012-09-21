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

  class SampleGenerator extends BasicGenerator

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
}
