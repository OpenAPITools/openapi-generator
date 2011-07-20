package com.wordnik.codegen.java;

import com.wordnik.codegen.config.DataTypeMapper;
import com.wordnik.codegen.config.ServiceAndMethodNameGenerator;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * User: ramesh
 * Date: 5/31/11
 * Time: 7:03 AM
 */
public class JavaDataTypeMapper implements DataTypeMapper {

    public static Map<String, String> primitiveValueMap = new HashMap<String, String>();
    static{
        primitiveValueMap.put("string", "String");
        primitiveValueMap.put("String", "String");
        primitiveValueMap.put("int", "int");
        primitiveValueMap.put("integer", "int");
        primitiveValueMap.put("Integer", "int");
        primitiveValueMap.put("boolean", "boolean");
        primitiveValueMap.put("Boolean", "boolean");
        primitiveValueMap.put("long", "long");
        primitiveValueMap.put("Long", "long");
        primitiveValueMap.put("float", "float");
        primitiveValueMap.put("Float", "float");
        primitiveValueMap.put("Date", "Date");
        primitiveValueMap.put("date", "Date");
    }

    public static Map<String, String> primitiveObjectMap = new HashMap<String, String>();
    static{
        primitiveObjectMap.put("string", "String");
        primitiveObjectMap.put("String", "String");
        primitiveObjectMap.put("int", "Integer");
        primitiveObjectMap.put("integer", "Integer");
        primitiveObjectMap.put("Integer", "Integer");
        primitiveObjectMap.put("boolean", "Boolean");
        primitiveObjectMap.put("Boolean", "Boolean");
        primitiveObjectMap.put("long", "Long");
        primitiveObjectMap.put("Long", "Long");
        primitiveObjectMap.put("float", "Float");
        primitiveObjectMap.put("Float", "Float");
        primitiveObjectMap.put("Date", "Date");
        primitiveObjectMap.put("date", "Date");
    }

    private ServiceAndMethodNameGenerator nameGenerator = new JavaServiceAndMethodNameGenerator();

    public boolean isPrimitiveType(String type) {
    	if(type.equalsIgnoreCase("String") || type.equalsIgnoreCase("int") || type.equalsIgnoreCase("integer") ||
    			type.equalsIgnoreCase("boolean") || type.equalsIgnoreCase("float")|| type.equalsIgnoreCase("long") ){
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
    		return nameGenerator.convertToClassNameFormat(type);
    	}
    }

    public String getListReturnType(String typeClass) {
        return "List<"+nameGenerator.convertToClassNameFormat(typeClass)+">";
    }

    public String getReturnTypeForVoidMethods() {
        return "void";
    }

    public String getMapReturnType(String typeClass) {
        return "Map<"+nameGenerator.convertToClassNameFormat(typeClass)+">";
    }

    public String getListInitialization(String typeClass) {
        return " new ArrayList<"+nameGenerator.convertToClassNameFormat(typeClass)+">()";
    }

    public String getMapInitialization(String typeClass) {
        return " new HashMap<"+nameGenerator.convertToClassNameFormat(typeClass)+">()";
    }

    public List<String> getListImports() {
        List<String> imports = new ArrayList<String>();
        imports.add("java.util.List");
        imports.add("java.util.ArrayList");
        return imports;
    }

    public List<String> getMapImports() {
        List<String> imports = new ArrayList<String>();
        imports.add("java.util.Map");
        imports.add("java.util.HashMap");
        return imports;
    }


    public List<String> getDateImports() {
        List<String> imports = new ArrayList<String>();
        imports.add("java.util.Date");
        return imports;
    }

	/**
	 * Gets the short name of the class the class.
	 * Input can be MAP, LIST or regular string. In case of map or list the class name will be class name
	 * that map or list is returning.
	 * @param type
	 * @return
	 */
    public String getReturnClassType(String type) {
    	String classShortName = "";
    	if(type.startsWith("List[")){
    		classShortName = type.substring(5, type.length()-1);
    		classShortName =  getObjectType(classShortName, true);
    	}else if (type.startsWith("Map[")) {
    		classShortName = type.substring(4, type.length()-1);
    		classShortName =  getObjectType(classShortName, true);
    	}else if (type.equals("ok")) {
    		classShortName = "void";
    	}else{
    		classShortName =  getObjectType(type, true);
    	}
    	return classShortName;
    }

	/**
	 * Gets the short name of the class the class.
	 * Input can be MAP, LIST or regular string. In case of map or list the class name will be class name
	 * that map or list is returning.
	 * @param type
	 * @return
	 */
    public String getReturnValueType(String type) {
    	if(type.equalsIgnoreCase("void")|| type.equalsIgnoreCase("ok")){
    		return "void";
    	}
    	String classShortName = "";
    	if(type.startsWith("List[")){
    		classShortName = type.substring(5, type.length()-1);
    		classShortName =  "List<"+getObjectType(classShortName, true)+">";
    	}else if (type.startsWith("Map[")) {
    		classShortName = type.substring(4, type.length()-1);
    		classShortName =  "List<"+getObjectType(classShortName, true) +">";
    	}else{
    		classShortName =  getObjectType(type, true);
    	}
    	return classShortName;
    }
}
