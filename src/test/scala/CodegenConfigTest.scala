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

import com.wordnik.swagger.codegen.util._
import com.wordnik.swagger.codegen.language._
import com.wordnik.swagger.codegen.PathUtil

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import scala.reflect.BeanProperty

@RunWith(classOf[JUnitRunner])
class CodegenConfigTest extends FlatSpec with ShouldMatchers {
  class SampleCodegenConfig extends CodegenConfig with PathUtil {
    override def packageName = "com.test"
    override def templateDir = "src/test/resources/sampleConfigTemplates"
    override def destinationDir = {
      val tmpFile = java.io.File.createTempFile("test",".tmp")
      tmpFile.delete; tmpFile.mkdir
      tmpFile.deleteOnExit
      tmpFile.toString
    }
    override def escapeReservedWord(word: String) = "`" + word + "`"
    override def typeMapping = Map("int" -> "integer")
    override def invokerPackage = Some("com.wordnik.something")
    override def apiPackage = Some("com.wordnik.api")
    override def modelPackage = Some("com.wordnik.models")
    override def reservedWords = Set("special")
    override def importMapping = super.importMapping ++ Map("User" -> "com.mypackage.User")
  }

  val config = new SampleCodegenConfig

  behavior of "PathUtil"
  /*
   * We will take an api in the spec and create an API name from it
   */
  it should "convert an api name" in {
    config.toApiName("fun") should be ("FunApi")
  }

  /*
   * We need to generate an API name from the resource path,
   * i.e. /foo will follow rules to become FooApi
  */
  it should "convert a path" in {
    config.apiNameFromPath("/foo/bar/cats/dogs") should be ("FooApi")
  }

  behavior of "CodegenConfig"
  /*
   * A response of type "void" will turn into a declaration of None
   * for the template generator
   */
  it should "process a response declaration" in {
    config.processResponseDeclaration("void") should be (None)
  }

  /*
   * if a response declaration is valid as-is, it will be
   * unchanged
   */
  it should "process an unchanged response" in {
    config.processResponseDeclaration("string") should be (Some("string"))
  }

  /*
   * if a typeMapping is configured, the response type declaration
   * from a method should be translated
   */
  it should "process an mapped response type" in {
    config.processResponseDeclaration("int") should be (Some("integer"))
  }

  /*
   * returns the invoker package from the config
   */
  it should "get the invoker package" in {
    config.invokerPackage should be (Some("com.wordnik.something"))
  }

  /*
   * returns the api package
   */
  it should "get the api package" in {
    config.apiPackage should be (Some("com.wordnik.api"))
  }

  /*
   * returns the model package
   */
  it should "get the model package" in {
    config.modelPackage should be (Some("com.wordnik.models"))
  }

  /*
   * types are mapped between swagger types and language-specific
   * types
   */
  it should "convert to a declared type" in {
    config.toDeclaredType("int") should be ("integer")
  }

  /*
   * codegen should honor special imports to avoid generating
   * classes
   */
  it should "honor the import mapping" in {
    config.importMapping("User") should be ("com.mypackage.User")
  }

  /*
   * reserved words should be treated appropriately by the config,
   * either by quoting or manipulating them with a prefix/suffix
   */
  it should "quote a reserved var name" in {
    config.toVarName("special") should be ("`special`")
  }
}
