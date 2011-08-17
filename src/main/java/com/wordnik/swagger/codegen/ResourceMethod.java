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

import com.wordnik.swagger.codegen.resource.Model;

import java.util.List;

public class ResourceMethod {

    private String title;

	private String description;

	private List<MethodArgument> arguments;
	
	private List<MethodArgument> queryParameters;
	
	private List<MethodArgument> pathParameters;
	
	private String returnValue;
	
	private String returnClassName;
	
	private String exceptionDescription;
	
	private List<String> argumentDefinitions;
	
	private List<String> argumentNames;	
	
	private String name;
	
	private boolean authToken;
	
	private String resourcePath;
	
	private String methodType;
	
	private boolean postObject;
	
	private Model inputModel;

    private Model outputWrapperModel;

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public List<MethodArgument> getArguments() {
		return arguments;
	}

	public void setArguments(List<MethodArgument> arguments) {
		this.arguments = arguments;
	}

	public List<MethodArgument> getQueryParameters() {
		return queryParameters;
	}

	public void setQueryParameters(List<MethodArgument> queryParameters) {
		this.queryParameters = queryParameters;
	}

	public List<MethodArgument> getPathParameters() {
		return pathParameters;
	}

	public void setPathParameters(List<MethodArgument> pathParameters) {
		this.pathParameters = pathParameters;
	}
	
	public String getReturnValue() {
		return returnValue;
	}

	public void setReturnValue(String returnValue) {
		this.returnValue = returnValue;
	}

	public String getReturnClassName() {
		return returnClassName;
	}

	public void setReturnClassName(String returnClassName) {
		this.returnClassName = returnClassName;
	}

	public String getExceptionDescription() {
		return exceptionDescription;
	}

	public void setExceptionDescription(String exceptionDescription) {
		this.exceptionDescription = exceptionDescription;
	}

	public List<String> getArgumentDefinitions() {
		return argumentDefinitions;
	}

	public void setArgumentDefinitions(List<String> argumentDefinitions) {
		this.argumentDefinitions = argumentDefinitions;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public boolean isAuthToken() {
		return authToken;
	}

	public void setAuthToken(boolean authToken) {
		this.authToken = authToken;
	}

	public String getResourcePath() {
		return resourcePath;
	}

	public void setResourcePath(String resourcePath) {
		this.resourcePath = resourcePath;
	}

	public String getMethodType() {
		return methodType;
	}

	public void setMethodType(String methodType) {
		this.methodType = methodType;
	}

	public boolean isPostObject() {
		return postObject;
	}

	public void setPostObject(boolean postObject) {
		this.postObject = postObject;
	}

	public boolean isResponseVoid() {
		return (this.getReturnClassName().equalsIgnoreCase("void"));
	}

	public Model getInputModel() {
		return inputModel;
	}

	public void setInputModel(Model inputModel) {
		this.inputModel = inputModel;
	}

	public List<String> getArgumentNames() {
		return argumentNames;
	}

	public void setArgumentNames(List<String> argumentNames) {
		this.argumentNames = argumentNames;
	}
	
	public boolean getHasArguments() {
		if(this.getArgumentNames() != null && this.getArgumentNames().size() > 0){
			return true;
		}
		return false;
	}
	
	public boolean isReturnValueList() {
		if(this.getReturnValue().startsWith("List")){
			return true;
		}
		return false;
	}

    public void setOutputWrapperModel(Model outputWrapperModel) {
        this.outputWrapperModel = outputWrapperModel;
    }

    public Model getOutputWrapperModel() {
        return outputWrapperModel;
    }
}
