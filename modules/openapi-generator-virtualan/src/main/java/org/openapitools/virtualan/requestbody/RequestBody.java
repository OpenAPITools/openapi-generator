/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.virtualan.requestbody;

import java.util.Collection;

import com.fasterxml.jackson.databind.ObjectMapper;

public class RequestBody {

	Object actualInput;
	String inputRequest;
	String expectedInput;
	Collection<String> excludeList;
	Class inputObjectType;
	String inputObjectTypeName;
	ObjectMapper objectMapper;
	

	public String getInputObjectTypeName() {
		return inputObjectTypeName;
	}
	public void setInputObjectTypeName(String inputObjectTypeName) {
		this.inputObjectTypeName = inputObjectTypeName;
	}
	
	
	
	public String getInputRequest() {
		return inputRequest;
	}
	public void setInputRequest(String inputRequest) {
		this.inputRequest = inputRequest;
	}
	public Object getActualInput() {
		return actualInput;
	}
	public void setActualInput(Object actualInput) {
		this.actualInput = actualInput;
	}
	public String getExpectedInput() {
		return expectedInput;
	}
	public void setExpectedInput(String expectedInput) {
		this.expectedInput = expectedInput;
	}
	public Collection<String> getExcludeList() {
		return excludeList;
	}
	public void setExcludeList(Collection<String> excludeList) {
		this.excludeList = excludeList;
	}
	public Class getInputObjectType() {
		return inputObjectType;
	}
	public void setInputObjectType(Class inputObjectType) {
		this.inputObjectType = inputObjectType;
	}
	public ObjectMapper getObjectMapper() {
		return objectMapper;
	}
	public void setObjectMapper(ObjectMapper objectMapper) {
		this.objectMapper = objectMapper;
	}

	
}
