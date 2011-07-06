package com.wordnik.codegen;

import com.wordnik.codegen.config.CodeGenConfig;

public class Argument {
	
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

	public void setInputModelClassArgument(String inputModelClass, CodeGenConfig config) {
		this.inputModelClassArgument = config.getNameGenerator().convertToMethodNameFormat(inputModelClass);
        if(name != null) {
            methodNameFromModelClass = config.getNameGenerator().createGetterMethodName(inputModelClassArgument, name);
        }
	}

    public String getMethodNameFromModelClass() {
        return methodNameFromModelClass;
    }
}
