/**
 *  Copyright 2011 Wordnik, Inc.
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

package com.wordnik.swagger.codegen.config.scala

import com.wordnik.swagger.codegen.config.DataTypeMappingProvider

import com.wordnik.swagger.codegen.config.NamingPolicyProvider
import com.wordnik.swagger.codegen.config.common.CamelCaseNamingPolicyProvider

import scala.collection.mutable._
import scala.collection.JavaConversions._

object ScalaDataTypeMappingProvider {
  val primitiveValueMap = Map("string" -> "String",
    "String" -> "String",
    "int" -> "int",
    "integer" -> "int",
    "Integer" -> "int",
    "boolean" -> "boolean",
    "Boolean" -> "boolean",
    "long" -> "long",
    "Long" -> "long",
    "float" -> "float",
    "Float" -> "float",
    "Date" -> "Date",
    "date" -> "Date",
    "byte" -> "Byte",
     "Byte" -> "Byte")

  val primitiveObjectMap = Map("string" -> "String",
    "String" -> "String",
    "java.lang.String" -> "String",
    "int" -> "Integer",
    "integer" -> "Integer",
    "Integer" -> "Integer",
    "java.lang.Integer" -> "Integer",
    "boolean" -> "Boolean",
    "Boolean" -> "Boolean",
    "java.lang.Boolean" -> "Boolean",
    "long" -> "Long",
    "Long" -> "Long",
    "java.lang.Long" -> "Long",
    "float" -> "Float",
    "Float" -> "Float",
    "java.lang.Float" -> "Float",
    "Date" -> "Date",
    "date" -> "Date",
    "byte" -> "Byte",
    "java.util.Date" -> "Date")
  val reservedWordMapper = new ScalaReservedWordMapper
}

class ScalaDataTypeMappingProvider extends DataTypeMappingProvider {
  import ScalaDataTypeMappingProvider._

  val nameGenerator = new CamelCaseNamingPolicyProvider()

  def isPrimitiveType(input: String): Boolean = {
    ScalaDataTypeMappingProvider.primitiveObjectMap.contains(input) match {
      case true => true
      case _ => false
    }
  }

  def getListReturnTypeSignature(typeClass: String): String = {
    return "List[" + nameGenerator.applyClassNamingPolicy(typeClass) + "]";
  }

  def getMapReturnTypeSignature(typeClass: String): String = {
    return "Map[" + nameGenerator.applyClassNamingPolicy(typeClass) + "]";
  }

  def getSetReturnTypeSignature(typeClass: String): String = {
    return "Set[" + nameGenerator.applyClassNamingPolicy(typeClass) + "]";
  }

  def getArrayReturnTypeSignature(typeClass: String): String = {
    return "Array["+nameGenerator.applyClassNamingPolicy(typeClass) + "]";
  }

  def generateVariableInitialization(typeClass:String):String = "=_"

  def generateListInitialization(typeClass: String): String = {
    return " new ListBuffer[" + nameGenerator.applyClassNamingPolicy(typeClass) + "]";
  }

  def generateMapInitialization(typeClass: String): String = {
    return " new HashMap[" + nameGenerator.applyClassNamingPolicy(typeClass) + "]";
  }

  def generateSetInitialization(typeClass: String): String = {
    return " new HashSet[" + nameGenerator.applyClassNamingPolicy(typeClass) + "]";
  }

  def generateArrayInitialization(typeClass: String): String = {
    return " null ";
  }

  def getListIncludes(): java.util.List[String] = {
    List("scala.collection.mutable.ListBuffer")
  }

  def getMapIncludes(): java.util.List[String] = {
    List("java.util.Map", "java.util.HashMap")
  }

  def getSetIncludes: java.util.List[String] = {
    List("java.util.Set", "java.util.HashSet")
  }

  def getDateIncludes: java.util.List[String] = {
    List("java.util.Date")
  }

  /**
   * Gets the short name of the class the class.
   * Input can be MAP, LIST or regular string. In case of map or list the class name will be class name
   * that map or list is returning.
   * @param type
   * @return
   */
  def getGenericType(inputType: String): String = {
    var classShortName = ""
    if (inputType.startsWith("List[")) {
      classShortName = inputType.substring(5, inputType.length() - 1);
      classShortName = getClassType(classShortName, true);
    } else if (inputType.startsWith("Map[")) {
      classShortName = inputType.substring(4, inputType.length() - 1)
      classShortName = getClassType(classShortName, true)
    } else if (inputType.startsWith("Set[")) {
      classShortName = inputType.substring(4, inputType.length() - 1)
      classShortName = getClassType(classShortName, true)
    }else if (inputType.startsWith("Array[")) {
      classShortName = inputType.substring(6, inputType.length() - 1)
      classShortName = getClassType(classShortName, true)
    }else if (inputType.equalsIgnoreCase("ok")) {
      classShortName = ""
    } else {
      classShortName = getClassType(inputType, true)
    }
    classShortName
  }

  /**
   * Returns the syntax for defintion of an object of type and name
   *
   * @param argumentType
   * @param argumentName
   * @return
   */
  def getArgumentDefinition(argumentType: String, argumentName: String): String = {
    reservedWordMapper.translate(argumentName) + ":" + argumentType
  }

  /**
   * Gets the class of the expected return value for a type string. Examples of type Strings are int, User, List[User]
   * If the type string is a collection type like a map or list the string value returned would be the class
   * that map or list is returning.
   *
   * @param type
   * @return
   */
  def getClassType(input: String, primitiveObject: Boolean): String = {
    if (input.equalsIgnoreCase("void") || input.equalsIgnoreCase("ok")) {
      ""
    } else {
      var classShortName = ""
      if (input.startsWith("List[")) {
        classShortName = input.substring(5, input.length() - 1);
        classShortName = "List[" + getClassType(classShortName, true) + "]";
      } else if (input.startsWith("Map[")) {
        classShortName = input.substring(4, input.length() - 1);
        val mapTypes:Array[String] = classShortName.split(",");
        classShortName = "Map[" +  getClassType(mapTypes(0), true) + "," + getClassType(mapTypes(1), true) + "]";
      } else if (input.startsWith("Set[")) {
        classShortName = input.substring(4, input.length() - 1);
        classShortName = "Set[" + getClassType(classShortName, true) + "]";
      } else if (input.startsWith("Array[")) {
        classShortName = input.substring(6, input.length() - 1);
        classShortName = "Array["+getClassName(classShortName, true) + "]";
      } else {
        classShortName = getClassName(input, true);
      }
      classShortName
    }
  }

  /**
   * If the data type is primitive and it is expecting object structure then return primitive objects
   * else return primitive types
   * @param type
   * @param primitiveObject -- indicates if the object is primitive or not
   * @return
   */
  def getClassName(input: String, primitiveObject: Boolean): String = {
    isPrimitiveType(input) match {
      case true => {
        if (primitiveObject) {
          ScalaDataTypeMappingProvider.primitiveObjectMap(input)
        } else {
          ScalaDataTypeMappingProvider.primitiveValueMap(input)
        }
      }
      case _ => nameGenerator.applyClassNamingPolicy(input)
    }
  }
}
