package org.openapitools.virtualan.model;

public class APIResponse {

	private String code;
	private String objectType;
	private String objectValue;
	private String objectMessage;

	public APIResponse() {

	}

	public APIResponse(String code, String objectType, String objectValue, String objectMessage) {
		super();
		this.code = code;
		this.objectType = objectType;
		this.objectValue = objectValue;
		this.objectMessage = objectMessage;
	}

	public String getCode() {
		return code;
	}

	public void setCode(String code) {
		this.code = code;
	}

	public String getObjectType() {
		return objectType;
	}

	public void setObjectType(String objectType) {
		this.objectType = objectType;
	}

	public String getObjectValue() {
		return objectValue;
	}

	public void setObjectValue(String objectValue) {
		this.objectValue = objectValue;
	}

	public String getObjectMessage() {
		return objectMessage;
	}

	public void setObjectMessage(String objectMessage) {
		this.objectMessage = objectMessage;
	}

}
