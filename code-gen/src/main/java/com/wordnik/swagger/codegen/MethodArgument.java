package com.wordnik.swagger.codegen;

import com.wordnik.swagger.codegen.config.NamingPolicyProvider;
import com.wordnik.swagger.codegen.config.NamingPolicyProvider;

public class MethodArgument {
	
	public static String ARGUMENT_STRING = "String";
	public static String ARGUMENT_INTEGER = "int";
	public static String ARGUMENT_OBJECT = "Object";
	
	private String name;
	
	private String description;
	
	private String dataType;
	
	private String allowedValues;

	private String inputModelClassArgument;
    private String methodNameFromModelClass;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getDataType() {
		return dataType;
	}

	public void setDataType(String dataType) {
		this.dataType = dataType;
	}

	public String getAllowedValues() {
		return allowedValues;
	}

	public void setAllowedValues(String allowedValues) {
		this.allowedValues = allowedValues;
	}

	public String getInputModelClassArgument() {
		return inputModelClassArgument;
	}

	public void setInputModelClassArgument(String inputModelClass, NamingPolicyProvider nameGenerator) {
		this.inputModelClassArgument = nameGenerator.applyMethodNamingPolicy(inputModelClass);
        if(name != null) {
            methodNameFromModelClass = nameGenerator.createGetterMethodName(inputModelClassArgument, name);
        }
	}

    public String getMethodNameFromModelClass() {
        return methodNameFromModelClass;
    }
}
