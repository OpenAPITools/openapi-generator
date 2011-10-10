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
package com.wordnik.swagger.codegen;

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
    private String defaultValue;
    private boolean required;

    public String getDefaultValue(){
    	return defaultValue;
    }
    
    public void setDefaultValue(String defaultValue){
    	this.defaultValue = defaultValue;
    }

    public boolean isRequired() {
    	return required;
    }

    public void setRequired(boolean required){
    	this.required = required;
    }

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
