/**
 *  Copyright 2014 Wordnik, Inc.
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

import com.wordnik.swagger.codegen.Codegen
import com.wordnik.swagger.codegen.BasicJavaGenerator
import com.wordnik.swagger.codegen.model._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.Matchers 

import scala.beans.BeanProperty
import scala.collection.mutable.{ HashMap, LinkedHashMap }

@RunWith(classOf[JUnitRunner])
class CodegenTest extends FlatSpec with Matchers {

  val subject = new Codegen(new BasicJavaGenerator)
  
  val testOp = new Operation("GET",
      "List All Contacts",
      "",
      "Array[ContactData]",
      "listContacts",
      0,
      List.empty,
      List.empty,
      List.empty,
      List.empty,
      //query param
      List(new Parameter("Name", Some("name"), Some("null"), false, false, "String", AnyAllowableValues, "query", None)),
      List.empty,
      None)

  val testModel = new Model("Contact",
      "Contact",
      "Contact",
      //required field
      LinkedHashMap("Name" -> new ModelProperty("String", "String", 0, true, None, AnyAllowableValues, None)),
      None,
      None,
      None)

  behavior of "Codegen"
  /*
   * A return specified as "Array" should map to "List"
   */
  it should "recognize the returnContainer as a List" in {
    val map = subject.apiToMap("/contacts", testOp)
    map("returnContainer") should be (Some("List"))
  }

  /*
   * Field first on the query param should be true
   */
  it should "have a first field on first query param and should be true" in {
    val map = subject.apiToMap("/contacts", testOp)
    map("queryParams").asInstanceOf[List[HashMap[String, String]]].head("first") should be ("true")
  }

  /*
   * Field hasRequiredParams should be true
   */
  it should "have a hasRequiredParams field and should be true" in {
    val map = subject.modelToMap("Contact", testModel)
    map("hasRequiredParams") should be ("true")
  }
}

