import com.wordnik.swagger.core.util.JsonUtil
import com.wordnik.swagger.codegen.util._

import com.wordnik.swagger.core.Documentation

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import scala.collection.JavaConverters._
import scala.reflect.BeanProperty

@RunWith(classOf[JUnitRunner])
class ResourceExtractorTest extends FlatSpec with ShouldMatchers {
  val json = ScalaJsonUtil.getJsonMapper

  behavior of "ResourceExtractor"
  it should "get 3 apis from a resource listing" in {
  	// todo: change to return documentation object
    val jsonString = ResourceExtractor.extractListing("src/test/resources/petstore/resources.json")
    val resourceListing = json.readValue(jsonString, classOf[Documentation])
    resourceListing should not be(null)
    resourceListing.getApis.size should be (3)
  }
}

@RunWith(classOf[JUnitRunner])
class ApiExtractorTest extends FlatSpec with ShouldMatchers {
  val json = ScalaJsonUtil.getJsonMapper

  behavior of "ApiExtractor"
  it should "verify the deserialization of the store api" in {
    val resourceListing = json.readValue(ResourceExtractor.extractListing("src/test/resources/petstore/resources.json", None), classOf[Documentation])
    val docs = ApiExtractor.extractApiDocs("src/test/resources/petstore", resourceListing.getApis.asScala.toList)
    val m = docs.map(t => (t.resourcePath, t)).toMap
    val storeApi = m("/store")

    storeApi should not be (null)
    storeApi.getApis.size should be (2)

    val f = storeApi.getApis.asScala.map(m => (m.path, m)).toMap
    (f.keys.toSet & Set("/store.{format}/order/{orderId}","/store.{format}/order")).size should be (2)

    val storeOps = f("/store.{format}/order/{orderId}")
    val ops = storeOps.getOperations.asScala.map(o => (o.nickname, o)).toMap
    val getOrderById = ops("getOrderById")

    getOrderById should not be null

    getOrderById.httpMethod should be ("GET")
    getOrderById.getParameters.size should be (1)
    getOrderById.getErrorResponses.size should be (2)
  }
}

@RunWith(classOf[JUnitRunner])
class CoreUtilsTest extends FlatSpec with ShouldMatchers {
  val json = ScalaJsonUtil.getJsonMapper
  sys.props += "fileMap" -> "src/test/resources/petstore"

  behavior of "CoreUtils"

  it should "verify models are extracted" in {
    val jsonString = ResourceExtractor.extractListing("src/test/resources/petstore/resources.json")
    val resourceListing = json.readValue(jsonString, classOf[Documentation])

    val apis = ApiExtractor.extractApiDocs("src/test/resources/petstore", resourceListing.getApis.asScala.toList)

    val cu = CoreUtils.extractAllModels(apis)
    cu.size should be (5)

    (cu.keys.toSet & Set("User", "Tag", "Pet", "Category", "Order")).size should be (5)
  }

  it should "verify operation names" in {
    val jsonString = ResourceExtractor.extractListing("src/test/resources/petstore/pet.json")
    val petApi = json.readValue(jsonString, classOf[Documentation])
    val eps = petApi.getApis.asScala.map(api => (api.path, api)).toMap
    val ops = eps("/pet.{format}").getOperations.asScala.map(ep => (ep.nickname, ep)).toMap

    ops.size should be (2)

    (ops.keys.toSet & Set("addPet", "updatePet")).size should be (2)
  }
}