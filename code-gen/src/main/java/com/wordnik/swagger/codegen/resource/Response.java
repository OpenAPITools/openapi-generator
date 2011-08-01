package com.wordnik.swagger.codegen.resource;


import java.util.List;

/**
 * User: ramesh
 * Date: 3/31/11
 * Time: 7:55 AM
 */
public class Response {

    private String valueType;

    private String condition;

    private List<ErrorResponse> errorResponses;

	public String getValueType() {
		return valueType;
	}

	public void setValueType(String valueType) {
		this.valueType = valueType;
	}

	public String getCondition() {
		return condition;
	}

	public void setCondition(String condition) {
		this.condition = condition;
	}

	public List<ErrorResponse> getErrorResponses() {
		return errorResponses;
	}

	public void setErrorResponses(List<ErrorResponse> errorResponses) {
		this.errorResponses = errorResponses;
	}

    
}
