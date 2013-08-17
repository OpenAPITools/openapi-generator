/**
 *  Copyright 2013 Wordnik, Inc.
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
import com.wordnik.swagger.codegen.{BasicCSharpGenerator, PathUtil}
import com.wordnik.swagger.codegen.util._
import com.wordnik.swagger.codegen.language._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import scala.reflect.BeanProperty

@RunWith(classOf[JUnitRunner])
class BasicCSharpGeneratorTest extends FlatSpec with ShouldMatchers {
  val config = new BasicCSharpGenerator

  behavior of "BasicCSharpGenerator"
  /*
   * A response of type "void" will turn into a declaration of None
   * for the template generator
   */
  it should "perserve the name date" in {
    config.toVarName("date") should be ("date")
  }

 /*
   * arrays look nice
   */
  it should "process a string array" in {
    config.processResponseDeclaration("array[string]") should be (Some("List<string>"))
  }
}
