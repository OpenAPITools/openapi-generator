package org.openapitools.virtualan.model;

import java.util.Map;

public class MockServiceRequest {
	String resource = null;
		String operationId  = null;
		Map<String, String> params = null;
		Class inputObjectType = null;
		Object inputObject = null;;
	
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

