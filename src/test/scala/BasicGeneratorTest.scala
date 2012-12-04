/**
 *  Copyright 2012 Wordnik, Inc.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

import com.wordnik.swagger.codegen.BasicGenerator
import com.wordnik.swagger.codegen.util._
import com.wordnik.swagger.model._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import scala.collection.mutable.{LinkedHashMap, HashMap}

@RunWith(classOf[JUnitRunner])
class BasicGeneratorTest extends FlatSpec with ShouldMatchers {
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
    val resourceListing = ResourceExtractor.fetchListing("src/test/resources/petstore/resources.json")

    val subDocs = ApiExtractor.fetchApiListings("src/test/resources/petstore", resourceListing.apis)
    val allModels = new HashMap[String, Model]

    implicit val basePath = "http://localhost:8080/api"
    val generator = new SampleGenerator
    val ops = generator.extractApiOperations(subDocs, allModels)

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
    orderApi.parameters.size should be (1)
    orderApi.errorResponses.size should be (1)
  }

  it should "verify ops are grouped by path correctly" in {
    val resourceListing = ResourceExtractor.fetchListing("src/test/resources/petstore/resources.json")
    val subDocs = ApiExtractor.fetchApiListings("src/test/resources/petstore", resourceListing.apis)
    val allModels = new HashMap[String, Model]()

    implicit val basePath = "http://localhost:8080/api"
    val generator = new SampleGenerator
    val ops = generator.extractApiOperations(subDocs, allModels)
    val apiMap = generator.groupOperationsToFiles(ops)

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
    val model = sampleModel

    val bundle = generator.prepareModelMap(Map(model.id -> model)).head

    // inspect properties
    bundle("name") should be ("SampleObject")
    bundle("className") should be ("SampleObject")
    bundle("invokerPackage") should be (Some("com.wordnik.client.common"))
    bundle("package") should be (Some("com.wordnik.client.model"))

    // inspect models
    val modelList = bundle("models").asInstanceOf[List[(String, Model)]]
    modelList.size should be (1)
    modelList.head._1 should be ("SampleObject")
    modelList.head._2.getClass should be (classOf[Model])
  }

  it should "create a model file" in {
    implicit val basePath = "http://localhost:8080/api"
    val generator = new SampleGenerator

    val model = sampleModel
    val bundle = generator.prepareModelMap(Map(model.id -> model))
    val modelFile = generator.bundleToSource(bundle, generator.modelTemplateFiles.toMap).head

    val fileContents = modelFile._2
    fileContents.indexOf("case class SampleObject") should not be (-1)
    fileContents.indexOf("longValue: Long") should not be (-1)
    fileContents.indexOf("intValue: Int") should not be (-1)
    fileContents.indexOf("doubleValue: Double") should not be (-1)
    fileContents.indexOf("stringValue: String") should not be (-1)
    fileContents.indexOf("floatValue: Float") should not be (-1)
  }

  def sampleModel = {
    Model(
      "SampleObject",
      "SampleObject",
      LinkedHashMap(
        "stringValue" -> ModelProperty("string"),
        "intValue" -> ModelProperty("int"),
        "longValue" -> ModelProperty("long"),
        "floatValue" -> ModelProperty("float"),
        "doubleValue" -> ModelProperty("double")),
      Some("a sample object"))
  }
}

