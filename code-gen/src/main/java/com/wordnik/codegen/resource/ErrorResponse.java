package com.wordnik.codegen.resource;

/**
 * Created by IntelliJ IDEA.
 * User: ramesh
 * Date: 3/31/11
 * Time: 8:24 AM
 * To change this template use File | Settings | File Templates.
 */
public class ErrorResponse {

    private int code;

    private String reason;

	public int getCode() {
		return code;
	}

	public void setCode(int code) {
		this.code = code;
	}

	public String getReason() {
		return reason;
	}

	public void setReason(String reason) {
		this.reason = reason;
	}

    
}
