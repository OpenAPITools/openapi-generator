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
package com.wordnik.swagger.codegen.config.csharp;

import com.wordnik.swagger.codegen.config.DataTypeMappingProvider;
import com.wordnik.swagger.codegen.config.NamingPolicyProvider;
import com.wordnik.swagger.codegen.config.common.CamelCaseNamingPolicyProvider;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * User: marek-stoj
 * Date: 5/11/12
 * Time: 5:56 PM
 */
public class CSharpDataTypeMappingProvider implements DataTypeMappingProvider {

  private static final Map<String, String> _primitiveValueMap;
  private static final Map<String, String> _primitiveObjectMap;
  
  private NamingPolicyProvider _nameGenerator = new CSharpNamingPolicyProvider();

  static {
    _primitiveValueMap = new HashMap<String, String>();
    _primitiveValueMap.put("string", "string");
    _primitiveValueMap.put("String", "string");
    _primitiveValueMap.put("int", "int");
    _primitiveValueMap.put("integer", "int");
    _primitiveValueMap.put("Integer", "int");
    _primitiveValueMap.put("boolean", "bool");
    _primitiveValueMap.put("Boolean", "bool");
    _primitiveValueMap.put("long", "long");
    _primitiveValueMap.put("Long", "long");
    _primitiveValueMap.put("float", "float");
    _primitiveValueMap.put("Float", "float");
    _primitiveValueMap.put("Date", "DateTime");
    _primitiveValueMap.put("date", "DateTime");
    _primitiveValueMap.put("Byte", "byte");
    
    _primitiveObjectMap = new HashMap<String, String>();
    _primitiveObjectMap.put("string", "string");
    _primitiveObjectMap.put("String", "string");
    _primitiveObjectMap.put("java.lang.String", "string");
    _primitiveObjectMap.put("int", "int");
    _primitiveObjectMap.put("integer", "int");
    _primitiveObjectMap.put("Integer", "int");
    _primitiveObjectMap.put("java.lang.Integer", "int");
    _primitiveObjectMap.put("boolean", "bool");
    _primitiveObjectMap.put("Boolean", "bool");
    _primitiveObjectMap.put("java.lang.Boolean", "bool");
    _primitiveObjectMap.put("long", "long");
    _primitiveObjectMap.put("Long", "long");
    _primitiveObjectMap.put("java.lang.Long", "long");
    _primitiveObjectMap.put("float", "float");
    _primitiveObjectMap.put("Float", "float");
    _primitiveObjectMap.put("java.lang.Float", "float");
    _primitiveObjectMap.put("Date", "DateTime");
    _primitiveObjectMap.put("date", "DateTime");
    _primitiveObjectMap.put("java.util.Date", "DateTime");
    _primitiveObjectMap.put("byte", "byte");
  }
  
  @Override
  public boolean isPrimitiveType(String type) {
    if (_primitiveObjectMap.containsKey(type)) {
      return true;
    }
    return false;
  }

  @Override
  public String getListReturnTypeSignature(String typeClass) {
    return "List<" + _nameGenerator.applyClassNamingPolicy(typeClass) + ">";
  }

  @Override
  public String getMapReturnTypeSignature(String typeClass) {
    return "Dictionary<" + _nameGenerator.applyClassNamingPolicy(typeClass) + ">";
  }

  @Override
  public String getSetReturnTypeSignature(String typeClass) {
    return "HashSet<" + _nameGenerator.applyClassNamingPolicy(typeClass) + ">";
  }

  @Override
  public String getArrayReturnTypeSignature(String typeClass) {
    return _nameGenerator.applyClassNamingPolicy(typeClass) + "[]";
  }

  @Override
  public String generateListInitialization(String typeClass) {
    return " new List<" + _nameGenerator.applyClassNamingPolicy(typeClass) + ">()";
  }

  @Override
  public String generateMapInitialization(String typeClass) {
    return " new Dictionary<" + _nameGenerator.applyClassNamingPolicy(typeClass) + ">()";
  }

  @Override
  public String generateSetInitialization(String typeClass) {
    return " new HashSet<" + _nameGenerator.applyClassNamingPolicy(typeClass) + ">()";
  }

  @Override
  public String generateArrayInitialization(String typeClass) {
    return " null";
  }

  @Override
  public List<String> getListIncludes() {
    List<String> imports = new ArrayList<String>();
    
    imports.add("System.Collections.Generic");
    
    return imports;
  }

  @Override
  public List<String> getMapIncludes() {
    List<String> imports = new ArrayList<String>();
    
    imports.add("System.Collections.Generic");
    
    return imports;
  }

  @Override
  public List<String> getSetIncludes() {
    List<String> imports = new ArrayList<String>();
    
    imports.add("System.Collections.Generic");
    
    return imports;
  }

  @Override
  public List<String> getDateIncludes() {
    return new ArrayList<String>();
  }

  /**
   * Gets the short name of the class the class.
   * Input can be MAP, LIST or regular string. In case of map or list the class name will be class name
   * that map or list is returning.
   * @param type
   * @return
   */
  @Override
  public String getGenericType(String type) {
    String classShortName = "";
    
    if (type.startsWith("List[")) {
      classShortName = type.substring(5, type.length() - 1);
      classShortName = getClassType(classShortName, true);
    }
    else if (type.startsWith("Map[")) {
      classShortName = type.substring(4, type.length() - 1);
      classShortName = getClassType(classShortName, true);
    }
    else if (type.startsWith("Set[")) {
      classShortName = type.substring(4, type.length() - 1);
      classShortName = getClassType(classShortName, true);
    }
    else if (type.startsWith("Array[")) {
      classShortName = type.substring(6, type.length() - 1);
      classShortName = getClassType(classShortName, true);
    }
    else if (type.equalsIgnoreCase("ok")) {
      classShortName = "void";
    }
    else {
      classShortName = getClassType(type, true);
    }
    
    return classShortName;
  }

  /**
   * Returns the syntax for defintion of an object of type and name
   *
   * @param argumentType
   * @param argumentName
   * @return
   */
  @Override
  public String getArgumentDefinition(String argumentType, String argumentName) {
    return argumentType + " " + argumentName;
  }

  /**
   * Gets the class of the expected return value for a type string. Examples of type Strings are int, User, List[User]
   * If the type string is a collection type like a map or list the string value returned would be the class
   * that map or list is returning.
   *
   * @param type
   * @return
   */
  @Override
  public String getClassType(String type, boolean primitiveObject) {
    if (type.equalsIgnoreCase("void") || type.equalsIgnoreCase("ok")) {
      return "void";
    }
    
    String classShortName = "";
    
    if (type.startsWith("List[")) {
      classShortName = type.substring(5, type.length() - 1);
      classShortName = "List<" + getClassType(classShortName, true) + ">";
    }
    else if (type.startsWith("Map[")) {
      classShortName = type.substring(4, type.length() - 1);
      String[] mapTypes = classShortName.split(",");
      classShortName = "Map<" + getClassType(mapTypes[0], true) + "," + getClassType(mapTypes[1], true) + ">";
    }
    else if (type.startsWith("Set[")) {
      classShortName = type.substring(4, type.length() - 1);
      classShortName = "Set<" + getClassType(classShortName, true) + ">";
    }
    else if (type.startsWith("Array[")) {
      classShortName = type.substring(6, type.length() - 1);
      classShortName = getClassName(classShortName, true) + "[]";
    }
    else {
      classShortName = getClassName(type, true);
    }
    
    return classShortName;
  }

  /**
   * If the data type is primitive and it is expecting object structure then return primitive objects
   * else return primitive types
   * @param type
   * @param primitiveObject -- indicates if the object is primitive or not
   * @return
   */
  private String getClassName(String type, boolean primitiveObject) {
    if (isPrimitiveType(type)) {
      if (primitiveObject) {
        return _primitiveObjectMap.get(type);
      }
      else {
        return _primitiveValueMap.get(type);
      }
    }
    else {
      return _nameGenerator.applyClassNamingPolicy(type);
    }
  }

  @Override
  public String generateVariableInitialization(String typeClass) {
    return "";
  }

}
