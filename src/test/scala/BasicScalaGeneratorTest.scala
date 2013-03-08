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

import com.wordnik.swagger.model._
import com.wordnik.swagger.codegen.{BasicScalaGenerator, Codegen, PathUtil}
import com.wordnik.swagger.codegen.util._
import com.wordnik.swagger.codegen.language._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import scala.collection.mutable.HashMap

@RunWith(classOf[JUnitRunner])
class BasicScalaGeneratorTest extends FlatSpec with ShouldMatchers {
  val config = new BasicScalaGenerator

  behavior of "BasicScalaGenerator"
  /*
   * A response of type "void" will turn into a declaration of None
   * for the template generator
   */
  it should "process a response declaration" in {
  	config.processResponseDeclaration("void") should be (None)
  }

  /*
   * swagger strings are turned into scala Strings
   */
  it should "process a string response" in {
  	config.processResponseDeclaration("string") should be (Some("String"))
  }

  /*
   * swagger int is turned into scala Int
   */
  it should "process an unmapped response type" in {
  	config.processResponseDeclaration("int") should be (Some("Int"))
  }

  /*
   * returns the invoker package from the config
   */
  it should "get the invoker package" in {
  	config.invokerPackage should be (Some("com.wordnik.client.common"))
  }

  /*
   * returns the api package
   */
  it should "get the api package" in {
  	config.apiPackage should be (Some("com.wordnik.client.api"))
  }

  /*
   * returns the model package
   */
  it should "get the model package" in {
  	config.modelPackage should be (Some("com.wordnik.client.model"))
  }

  /*
   * types are mapped between swagger types and language-specific
   * types
   */
  it should "convert to a declared type" in {
    config.toDeclaredType("boolean") should be ("Boolean")
  	config.toDeclaredType("string") should be ("String")
    config.toDeclaredType("int") should be ("Int")
    config.toDeclaredType("float") should be ("Float")
    config.toDeclaredType("long") should be ("Long")
    config.toDeclaredType("double") should be ("Double")
    config.toDeclaredType("object") should be ("Any")
  }

  /*
   * declarations are used in models, and types need to be
   * mapped appropriately
   */
  it should "convert a string a declaration" in {
    val expected = Map("string" -> ("String", "_"),
      "int" -> ("Int", "0"),
      "float" -> ("Float", "0f"),
      "long" -> ("Long", "0L"),
      "double" -> ("Double", "0.0"),
      "object" -> ("Any", "_"))
    expected.map(e => {
      val model = ModelProperty(e._1)
      config.toDeclaration(model) should be (e._2)
    })
  }

  /*
   * codegen should honor special imports to avoid generating
   * classes
   */
  it should "honor the import mapping" in {
  	config.importMapping("Date") should be ("java.util.Date")
  }

  /*
   * single tick reserved words
   */
  it should "quote a reserved var name" in {
  	config.toVarName("package") should be ("`package`")
  }

  /*
   * support list declarations with string inner value and the correct default value
   */
   it should "create a declaration with a List of strings" in {
      val property = ModelProperty(
        "Array", 
        items=Some(ModelRef(`type`= "string")))
      val m = config.toDeclaration(property)
      m._1 should be ("List[String]")
      m._2 should be ("_")
   }

  /*
   * support list declarations with int inner value and the correct default value
   */
   it should "create a declaration with a List of ints" in {
      val property = ModelProperty(
        "Array", 
        items=Some(ModelRef(`type`= "int")))
      val m = config.toDeclaration(property)
      m._1 should be ("List[Int]")
      m._2 should be ("0")
   }

  /*
   * support list declarations with float inner value and the correct default value
   */
   it should "create a declaration with a List of floats" in {
      val property = ModelProperty(
        "Array", 
        items=Some(ModelRef(`type`= "float")))
      val m = config.toDeclaration(property)
      m._1 should be ("List[Float]")
      m._2 should be ("0f")
   }

  /*
   * support list declarations with double inner value and the correct default value
   */
   it should "create a declaration with a List of doubles" in {
      val property = ModelProperty(
        "Array", 
        items=Some(ModelRef(`type`= "double")))
      val m = config.toDeclaration(property)
      m._1 should be ("List[Double]")
      m._2 should be ("0.0")
   }

  /*
   * support list declarations with complex inner value and the correct default value
   */
   it should "create a declaration with a List of complex objects" in {
      val property = ModelProperty(
        "Array", 
        items=Some(ModelRef(`type`= "User")))
      val m = config.toDeclaration(property)
      m._1 should be ("List[User]")
      m._2 should be ("_")
   }

  it should "verify an api map with query params" in {
    val resourceListing = ResourceExtractor.fetchListing("src/test/resources/petstore/resources.json", None)
    val apis = ApiExtractor.extractApiOperations("src/test/resources/petstore", resourceListing.apis)
    val codegen = new Codegen(config)
    val petApi = apis.filter(doc => doc.resourcePath == "/pet").head

    val endpoint = petApi.apis.filter(api => api.path == "/pet.{format}/findByTags").head
    val operation = endpoint.operations.filter(op => op.httpMethod == "GET").head
    val m = codegen.apiToMap("http://my.api.com/api", operation)

    m("path") should be ("http://my.api.com/api")
    m("bodyParams").asInstanceOf[List[_]].size should be (0)
    m("httpMethod") should be ("GET")

    // Pet => NIKPet
    m("returnBaseType") should be (Some("Pet"))
    m("returnType") should be (Some("List[Pet]"))
    m("returnTypeIsPrimitive") should be (None)
    m("pathParams").asInstanceOf[List[_]].size should be (0)
    m("returnContainer") should be ("List")
    m("requiredParamCount") should be ("1")

    val queryParams = m("queryParams").asInstanceOf[List[_]]
    queryParams.size should be (1)

    val queryParam = queryParams.head.asInstanceOf[HashMap[String, _]]
    queryParam("type") should be ("query")
    queryParam("dataType") should be ("String")
    queryParam("required") should be ("true")
    queryParam("paramName") should be ("tags")
    queryParam("swaggerDataType") should be ("string")
    queryParam("allowMultiple") should be ("true")
    queryParam("defaultValue") should be (None)
  }

  it should "verify an api map with query params with default values" in {
    val resourceListing = ResourceExtractor.fetchListing("src/test/resources/petstore/resources.json", None)
    val apis = ApiExtractor.extractApiOperations("src/test/resources/petstore", resourceListing.apis)
    val codegen = new Codegen(config)
    val petApi = apis.filter(doc => doc.resourcePath == "/pet").head

    val endpoint = petApi.apis.filter(api => api.path == "/pet.{format}/findByStatus").head
    val operation = endpoint.operations.filter(op => op.httpMethod == "GET").head
    val m = codegen.apiToMap("http://my.api.com/api", operation)

    m("path") should be ("http://my.api.com/api")
    m("bodyParams").asInstanceOf[List[_]].size should be (0)
    m("httpMethod") should be ("GET")

    // Pet => Pet
    m("returnBaseType") should be (Some("Pet"))

    // problem here
    m("returnType") should be (Some("List[Pet]"))
    m("returnTypeIsPrimitive") should be (None)
    m("pathParams").asInstanceOf[List[_]].size should be (0)
    m("returnContainer") should be ("List")
    m("requiredParamCount") should be ("1")

    val queryParams = m("queryParams").asInstanceOf[List[_]]
    queryParams.size should be (1)

    val queryParam = queryParams.head.asInstanceOf[HashMap[String, _]]
    queryParam("type") should be ("query")
    queryParam("dataType") should be ("String")
    queryParam("required") should be ("true")
    queryParam("paramName") should be ("status")
    queryParam("swaggerDataType") should be ("string")
    queryParam("allowMultiple") should be ("true")
    queryParam("defaultValue") should be (Some("\"available\""))
    queryParam("allowableValues") should be (Some("LIST[available,pending,sold]"))
  }

  it should "create an api file" in {
    implicit val basePath = "http://localhost:8080/api"
    val codegen = new Codegen(config)
    val resourceListing = ResourceExtractor.fetchListing("src/test/resources/petstore/resources.json", None)

    val apis = ApiExtractor.extractApiOperations("src/test/resources/petstore", resourceListing.apis)
    val petApi = apis.filter(doc => doc.resourcePath == "/pet").head

    val endpoint = petApi.apis.filter(api => api.path == "/pet.{format}/findByTags").head
    val operation = endpoint.operations.filter(op => op.httpMethod == "GET").head
    val m = codegen.apiToMap("http://my.api.com/api", operation)

    val allModels = new HashMap[String, Model]
    val operations = config.extractApiOperations(List(petApi), allModels)

    val apiMap = config.groupOperationsToFiles(operations)
    val bundle = config.prepareApiBundle(apiMap)
    val apiFiles = config.bundleToSource(bundle, config.apiTemplateFiles.toMap)

    apiFiles.size should be (1)
    val file = apiFiles.head

    // verify the filename is set
    file._1.indexOf("""PetApi.scala""") should not be (-1)
    // verify the default value for status exists
    file._2.indexOf("""(status: String= "available")""") should not be (-1)
  }
}
