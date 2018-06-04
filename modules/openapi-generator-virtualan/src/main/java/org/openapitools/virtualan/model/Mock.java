package org.openapitools.virtualan.model;

import java.util.List;
import java.util.Map;

public class Mock {

	private long id;
	private String operationId;
	private String httpStatusCode;
	private String url;
	private String method;
	private Class inputObjectType;
	private String outputObjectType;
	private String input;
	private Object inputObject;
	private String output;
	private List<MockKeyValue> availableParams;
	private Map<String, APIResponse> responseType;
	private String excludeList;
	private String resource;
	private String desc;
	private MockStatus mockStatus;
	
	
	
	public String getResource() {
		return resource;
	}

	public void setResource(String resource) {
		this.resource = resource;
	}

	public Map<String, APIResponse> getResponseType() {
		return responseType;
	}

	public void setResponseType(Map<String, APIResponse> responseType) {
		this.responseType = responseType;
	}

	public String getExcludeList() {
		return excludeList;
	}

	public void setExcludeList(String excludeList) {
		this.excludeList = excludeList;
	}

	private Map<String, String> httpStatusMap;

	public Mock(long id, String operationId, String input, String output) {
		this.id = id;
		this.operationId = operationId;
		this.input = input;
		this.output = output;
	}

	public Map<String, String> getHttpStatusMap() {
		return httpStatusMap;
	}

	public void setHttpStatusMap(Map<String, String> httpStatusMap) {
		this.httpStatusMap = httpStatusMap;
	}

	public Mock() {
	}

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

	public List<MockKeyValue> getAvailableParams() {
		return availableParams;
	}

	public void setAvailableParams(List<MockKeyValue> availableParams) {
		this.availableParams = availableParams;
	}

	public String getOperationId() {
		return operationId;
	}

	public void setOperationId(String operationId) {
		this.operationId = operationId;
	}

	public String getInput() {
		return input;
	}

	public void setInput(String input) {
		this.input = input;
	}

	public String getOutput() {
		return output;
	}

	public void setOutput(String output) {
		this.output = output;
	}

	public String getUrl() {
		return url;
	}

	public void setUrl(String url) {
		this.url = url;
	}

	public String getMethod() {
		return method;
	}

	public void setMethod(String method) {
		this.method = method;
	}

	public String getHttpStatusCode() {
		return httpStatusCode;
	}

	public void setHttpStatusCode(String httpStatusCode) {
		this.httpStatusCode = httpStatusCode;
	}

	public Class getInputObjectType() {
		return inputObjectType;
	}

	public void setInputObjectType(Class inputObjectType) {
		this.inputObjectType = inputObjectType;
	}

	public String getOutputObjectType() {
		return outputObjectType;
	}

	public void setOutputObjectType(String outputObjectType) {
		this.outputObjectType = outputObjectType;
	}

	@Override
	public String toString() {
		return "MockTransferObject [id=" + id + ", operationId=" + operationId + ", httpStatusCode=" + httpStatusCode
				+ ", url=" + url + ", method=" + method + ", inputObjectType=" + inputObjectType + ", outputObjectType="
				+ outputObjectType + ", input=" + input + ", output=" + output + ", availableParams=" + availableParams
				+ ", excludeList=" + excludeList + ", httpStatusMap=" + httpStatusMap + "]";
	}

	public String getDesc() {
		return desc;
	}

	public void setDesc(String desc) {
		this.desc = desc;
	}

	public MockStatus getMockStatus() {
		return mockStatus;
	}

	public void setMockStatus(MockStatus mockStatus) {
		this.mockStatus = mockStatus;
	}

	public void setInputObject(Object inputObject) {
		this.inputObject = inputObject;
	}

	public Object getInputObject() {
		return inputObject;
	}
}
