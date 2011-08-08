package com.wordnik.swagger.testframework;

public class TestResource {

	private int id;
	
	private String name;
	
	private String httpMethod;
	
	private String path;
	
	private String suggestedMethodName;

	public int getId() {
		return id;
	}

	public void setId(int id) {
		this.id = id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getHttpMethod() {
		return httpMethod;
	}

	public void setHttpMethod(String method) {
		this.httpMethod = method;
	}

	public String getPath() {
		return path;
	}

	public void setPath(String path) {
		this.path = path;
	}

	public String getSuggestedMethodName() {
		return suggestedMethodName;
	}

	public void setSuggestedMethodName(String suggestedMethodName) {
		this.suggestedMethodName = suggestedMethodName;
	}
	
}
