package com.wordnik.swagger.codegen.config.as3;

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
public class As3DataTypeMappingProvider implements DataTypeMappingProvider {

    public static Map<String, String> primitiveValueMap = new HashMap<String, String>();
    static{
        primitiveValueMap.put("string", "String");
        primitiveValueMap.put("String", "String");
        primitiveValueMap.put("int", "Number");
        primitiveValueMap.put("integer", "int");
        primitiveValueMap.put("Integer", "int");
        primitiveValueMap.put("boolean", "Boolean");
        primitiveValueMap.put("Boolean", "Boolean");
        primitiveValueMap.put("long", "Number");
        primitiveValueMap.put("Long", "Number");
        primitiveValueMap.put("double", "Number");
        primitiveValueMap.put("Double", "Number");
        primitiveValueMap.put("float", "Number");
        primitiveValueMap.put("Float", "Number");
        primitiveValueMap.put("Date", "Date");
        primitiveValueMap.put("date", "Date");
        primitiveValueMap.put("byte", "byte");
        primitiveValueMap.put("Byte", "byte");


    }

    public static Map<String, String> primitiveObjectMap = new HashMap<String, String>();
    static{
        primitiveObjectMap.put("string", "String");
        primitiveObjectMap.put("String", "String");
        primitiveObjectMap.put("int", "int");
        primitiveObjectMap.put("integer", "int");
        primitiveObjectMap.put("Integer", "int");
        primitiveObjectMap.put("boolean", "Boolean");
        primitiveObjectMap.put("Boolean", "Boolean");
        primitiveObjectMap.put("long", "Number");
        primitiveObjectMap.put("Long", "Number");
        primitiveObjectMap.put("double", "Number");
        primitiveObjectMap.put("Double", "Number");
        primitiveObjectMap.put("float", "Number");
        primitiveObjectMap.put("Float", "Number");
        primitiveObjectMap.put("Date", "Date");
        primitiveObjectMap.put("date", "Date");
        primitiveObjectMap.put("byte", "byte");

    }

    private NamingPolicyProvider nameGenerator = new CamelCaseNamingPolicyProvider();

    public boolean isPrimitiveType(String type) {
    	if(type.equalsIgnoreCase("String") || type.equalsIgnoreCase("int") || type.equalsIgnoreCase("integer") || type.equalsIgnoreCase("double") ||
    			type.equalsIgnoreCase("boolean") || type.equalsIgnoreCase("float")|| type.equalsIgnoreCase("long") || type.equalsIgnoreCase("Number") ){
    		return true;
    	}
    	return false;
    }

    /**
     * If the data type is primitive and it is expecting object structure then return primitive objects
     * else return primitive types
     * @param type
     * @param primitiveObject -- indicates if the object is primitive or not
     * @return
     */
    public String getObjectType(String type, boolean primitiveObject) {
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

    public String getListReturnTypeSignature(String typeClass) {
        return "Array";
    }

    public String getReturnTypeForVoidMethods() {
        return "void";
    }

    public String getMapReturnTypeSignature(String typeClass) {
        return "Object";
    }

    public String getSetReturnTypeSignature(String typeClass) {
        return "Array";
    }

    public String getArrayReturnTypeSignature(String typeClass) {
        return "Array";
    }

    public String generateListInitialization(String typeClass) {
        return " new Array()";
    }

    public String generateMapInitialization(String typeClass) {
        return " new Object()";
    }

    public String generateSetInitialization(String typeClass) {
        return " new Array()";

    }

    public String generateArrayInitialization(String typeClass) {
        return " new Array()";
    }

    public List<String> getListIncludes() {
        List<String> imports = new ArrayList<String>();
        return imports;
    }

    public List<String> getMapIncludes() {
        List<String> imports = new ArrayList<String>();
        imports.add("flash.utils.Dictionary");
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
    public String getClassType(String type, boolean primitiveObject) {
    	String classShortName = "";
    	if(type.startsWith("List[")){
    		classShortName = type.substring(5, type.length()-1);
    		classShortName =  getObjectType(classShortName, true);
    	}else if (type.startsWith("Map[")) {
    		classShortName = type.substring(4, type.length()-1);
    		classShortName =  getObjectType(classShortName, true);
    	}else if (type.startsWith("Set[")) {
    		classShortName = type.substring(4, type.length()-1);
    		classShortName =  getObjectType(classShortName, true);
    	}else if (type.startsWith("Array[")) {
            classShortName = type.substring(6, type.length()-1);
            classShortName =  getObjectType(classShortName, true);
        }else if (type.equals("ok")) {
    		classShortName = "void";
    	}else{
    		classShortName =  getObjectType(type, true);
    	}
    	return classShortName;
    }

    public String getArgumentDefinition(String argumentType, String argumentName) {
        return argumentName + ": " + argumentType;
    }

    /**
	 * Gets the class of the expected return value for a type string. Examples of type Strings are int, User, List[User]
	 * If the type string is a collection type like a map or list the string value returned would be the class
	 * that map or list is returning.
     *
	 * @param type
	 * @return
	 */
    public String getGenericType(String type) {
    	if(type.equalsIgnoreCase("void")|| type.equalsIgnoreCase("ok")){
    		return "void";
    	}
    	String classShortName = "";
    	if(type.startsWith("List[") || type.startsWith("Map[") || type.startsWith("Set[") || type.startsWith("Array[") ){
    		classShortName = "Array";
    	}else{
    		classShortName =  getObjectType(type, true);
    	}
    	return classShortName;
    }
    
	@Override
	public String generateVariableInitialization(String typeClass) {
		return "";
	}
}
