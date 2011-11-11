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

package com.wordnik.swagger.codegen.config.python;

import com.wordnik.swagger.codegen.config.DataTypeMappingProvider;
import com.wordnik.swagger.codegen.config.NamingPolicyProvider;
import com.wordnik.swagger.codegen.config.common.CamelCaseNamingPolicyProvider;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * User: ramesh
 * Date: 5/31/11
 * Time: 7:03 AM
 */
public class PythonDataTypeMappingProvider implements DataTypeMappingProvider {

    public static Map<String, String> primitiveValueMap = new HashMap<String, String>();
    static{
        primitiveValueMap.put("string", "str");
        primitiveValueMap.put("String", "str");
        primitiveValueMap.put("int", "int");
        primitiveValueMap.put("integer", "int");
        primitiveValueMap.put("Integer", "int");
        primitiveValueMap.put("boolean", "bool");
        primitiveValueMap.put("Boolean", "bool");
        primitiveValueMap.put("long", "int");
        primitiveValueMap.put("Long", "int");
        primitiveValueMap.put("float", "float");
        primitiveValueMap.put("Float", "float");
        primitiveValueMap.put("Date", "str");
        primitiveValueMap.put("date", "str");
        primitiveValueMap.put("Double", "float");
        primitiveValueMap.put("double", "float");
    }

    public static Map<String, String> primitiveObjectMap = new HashMap<String, String>();
    static{
        primitiveObjectMap.put("string", "str");
        primitiveObjectMap.put("String", "str");
        primitiveObjectMap.put("java.lang.String", "str");
        primitiveObjectMap.put("int", "int");
        primitiveObjectMap.put("integer", "int");
        primitiveObjectMap.put("Integer", "int");
        primitiveObjectMap.put("java.lang.Integer", "int");
        primitiveObjectMap.put("bool", "bool");
        primitiveObjectMap.put("boolean", "bool");
        primitiveObjectMap.put("Boolean", "bool");
        primitiveObjectMap.put("java.lang.Boolean", "bool");
        primitiveObjectMap.put("long", "int");
        primitiveObjectMap.put("Long", "int");
        primitiveObjectMap.put("java.lang.Long", "int");
        primitiveObjectMap.put("float", "float");
        primitiveObjectMap.put("Float", "float");
        primitiveObjectMap.put("double", "float");
        primitiveObjectMap.put("Double", "float");
        primitiveObjectMap.put("java.lang.Float", "float");
        primitiveObjectMap.put("Date", "str");
        primitiveObjectMap.put("date", "str");
        primitiveObjectMap.put("java.util.Date", "str");
    }

    private NamingPolicyProvider nameGenerator = new CamelCaseNamingPolicyProvider();

    public boolean isPrimitiveType(String type) {
    	if(primitiveObjectMap.containsKey(type)){
    		return true;
    	}
    	return false;
    }

    public String getListReturnTypeSignature(String typeClass) {
        if (isPrimitiveType(typeClass)) {
            return "list<"+typeClass+">";
        } else {
            return "list<"+nameGenerator.applyClassNamingPolicy(typeClass)+">";
        }
    }

    public String getMapReturnTypeSignature(String typeClass) {
        return "dict<"+nameGenerator.applyClassNamingPolicy(typeClass)+">";
    }

    public String getSetReturnTypeSignature(String typeClass) {
        return "set<"+nameGenerator.applyClassNamingPolicy(typeClass)+">";
    }

    public String generateListInitialization(String typeClass) {
        return " list()";
    }

    public String generateMapInitialization(String typeClass) {
        return " dict()";
    }

    public String generateSetInitialization(String typeClass) {
        return " set()";
    }

    public List<String> getListIncludes() {
        List<String> imports = new ArrayList<String>();
        return imports;
    }

    public List<String> getMapIncludes() {
        List<String> imports = new ArrayList<String>();
        return imports;
    }

    public List<String> getSetIncludes() {
        List<String> imports = new ArrayList<String>();
        return imports;    }


    public List<String> getDateIncludes() {
        List<String> imports = new ArrayList<String>();
        return imports;
    }

	/**
	 * Gets the short name of the class the class.
	 * Input can be MAP, LIST or regular string. In case of map or list the class name will be class name
	 * that map or list is returning.
	 * @param type
	 * @return
	 */
    public String getGenericType(String type) {
    	String classShortName = "";
    	if(type.startsWith("List[")){
    		classShortName = type.substring(5, type.length()-1);
    		classShortName =  getClassType(classShortName, true);
    	}else if (type.startsWith("Map[")) {
    		classShortName = type.substring(4, type.length()-1);
    		classShortName =  getClassType(classShortName, true);
    	}else if (type.startsWith("Set[")) {
    		classShortName = type.substring(4, type.length()-1);
    		classShortName =  getClassType(classShortName, true);
    	}else if (type.equalsIgnoreCase("ok")) {
    		classShortName = "void";
    	}else{
    		classShortName =  getClassType(type, true);
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
    public String getClassType(String type, boolean primitiveObject) {
    	if(type.equalsIgnoreCase("void")|| type.equalsIgnoreCase("ok")){
    		return "void";
    	}
    	String classShortName = "";
    	if(type.startsWith("List[")){
    		classShortName = type.substring(5, type.length()-1);
    		classShortName =  "list<"+ getClassName(classShortName, true)+">";
    	}else if (type.startsWith("Map[")) {
    		classShortName = type.substring(4, type.length()-1);
    		classShortName =  "dict<"+ getClassName(classShortName, true) +">";
    	}else if (type.startsWith("Set[")) {
    		classShortName = type.substring(4, type.length()-1);
    		classShortName =  "set<"+ getClassName(classShortName, true) +">";
    	}else{
    		classShortName =  getClassName(type, true);
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
    	if(isPrimitiveType(type)){
	    	if(primitiveObject){
	    		return primitiveObjectMap.get(type);
	    	}else{
	    		return primitiveValueMap.get(type);
	    	}
    	}else{

    		return nameGenerator.applyClassNamingPolicy(type);
    	}
    }
    
    @Override
  	public String generateVariableInitialization(String typeClass) {
  		return "";
  	}

}
