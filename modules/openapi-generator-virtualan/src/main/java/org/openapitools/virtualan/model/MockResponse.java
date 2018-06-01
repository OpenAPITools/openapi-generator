package org.openapitools.virtualan.model;

public class MockResponse {
	String output;
	String httpStatusCode;

	public MockResponse(String output, String httpStatusCode) {
		this.output = output;
		this.httpStatusCode = httpStatusCode;
	}

	public String getOutput() {
		return output;
	}

	public void setOutput(String output) {
		this.output = output;
	}

	public String getHttpStatusCode() {
		return httpStatusCode;
	}

	public void setHttpStatusCode(String httpStatusCode) {
		this.httpStatusCode = httpStatusCode;
	}

	@Override
	public String toString() {
		return "MockResponse [output=" + output + ", httpStatusCode=" + httpStatusCode + "]";
	}

}
