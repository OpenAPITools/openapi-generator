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

package org.openapitools.virtualan.model;

import java.util.Map;

public class MockServiceRequest {
	String resource = null;
		String operationId  = null;
		Map<String, String> params = null;
		Class inputObjectType = null;
		Object inputObject = null;
	
		public Object getInputObject() {
			return inputObject;
		}
	
		public void setInputObject(Object inputObject) {
			this.inputObject = inputObject;
		}
	
		public String getResource() {
			return resource;
		}
	
		public void setResource(String resource) {
			this.resource = resource;
		}
	
		public String getOperationId() {
			return operationId;
		}
	
		public void setOperationId(String operationId) {
			this.operationId = operationId;
		}
	
		public Map<String, String> getParams() {
			return params;
		}
	
		public void setParams(Map<String, String> params) {
			this.params = params;
		}
	
		public Class getInputObjectType() {
			return inputObjectType;
		}
	
		public void setInputObjectType(Class inputObjectType) {
			this.inputObjectType = inputObjectType;
		}
}

