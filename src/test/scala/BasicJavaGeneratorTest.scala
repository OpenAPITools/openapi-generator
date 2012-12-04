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
import com.wordnik.swagger.codegen.{BasicJavaGenerator, PathUtil}
import com.wordnik.swagger.codegen.util._
import com.wordnik.swagger.codegen.language._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import scala.reflect.BeanProperty

@RunWith(classOf[JUnitRunner])
class BasicJavaGeneratorTest extends FlatSpec with ShouldMatchers {
  val config = new BasicJavaGenerator

  behavior of "BasicJavaGenerator"
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
  	config.processResponseDeclaration("int") should be (Some("Integer"))
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
  	config.toDeclaredType("string") should be ("String")
    config.toDeclaredType("int") should be ("Integer")
    config.toDeclaredType("float") should be ("Float")
    config.toDeclaredType("long") should be ("Long")
    config.toDeclaredType("double") should be ("Double")
    config.toDeclaredType("object") should be ("Object")
  }

  /*
   * declarations are used in models, and types need to be
   * mapped appropriately
   */
  it should "convert a string a declaration" in {
    val expected = Map(
      "string" -> ("String", "null"),
      "int" -> ("Integer", "null"),
      "float" -> ("Float", "null"),
      "long" -> ("Long", "null"),
      "double" -> ("Double", "null"),
      "object" -> ("Object", "null"))
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
   * fail on reserved words (java doesn't allow quoting)
   */
  it should "quote a reserved var name" in {
  	val thrown = intercept[Exception] {
      config.toVarName("package")
    }
    thrown should not be (null)
  }

  /*
   * support list declarations with string inner value and the correct default value
   */
   it should "create a declaration with a List of strings" in {
      val property = ModelProperty(
        "Array", 
        items=Some(ModelRef(`type`= "string")))
      val m = config.toDeclaration(property)
      m._1 should be ("List<String>")
      m._2 should be ("new ArrayList<String>()")
   }

  /*
   * support list declarations with int inner value and the correct default value
   */
   it should "create a declaration with a List of ints" in {
      val property = ModelProperty(
        "Array", 
        items=Some(ModelRef(`type`= "int")))
      val m = config.toDeclaration(property)
      m._1 should be ("List<Integer>")
      m._2 should be ("new ArrayList<Integer>()")
   }

  /*
   * support list declarations with float inner value and the correct default value
   */
   it should "create a declaration with a List of floats" in {
      val property = ModelProperty(
        "Array", 
        items=Some(ModelRef(`type`= "float")))
      val m = config.toDeclaration(property)
      m._1 should be ("List<Float>")
      m._2 should be ("new ArrayList<Float>()")
   }

  /*
   * support list declarations with double inner value and the correct default value
   */
   it should "create a declaration with a List of doubles" in {
      val property = ModelProperty(
        "Array", 
        items=Some(ModelRef(`type`= "double")))
      val m = config.toDeclaration(property)
      m._1 should be ("List<Double>")
      m._2 should be ("new ArrayList<Double>()")
   }

  /*
   * support list declarations with complex inner value and the correct default value
   */
   it should "create a declaration with a List of complex objects" in {
      val property = ModelProperty(
        "Array", 
        items=Some(ModelRef(`type`= "User")))
      val m = config.toDeclaration(property)
      m._1 should be ("List<User>")
      m._2 should be ("new ArrayList<User>()")
   }
}
