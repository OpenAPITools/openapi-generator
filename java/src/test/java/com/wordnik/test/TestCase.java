package com.wordnik.test;

import java.util.List;
import java.util.Map;

import org.codehaus.jackson.map.annotate.JsonDeserialize;


/**
 * Test case executes individual test case. A given test suite has one or many test cases. 
 * @author ramesh
 *
 */
public class TestCase {
	
	private String name;
	
	private int id;
	
	private int resourceId;
	
	private Map<String, String> input;
	
	private String referenceInput;
	
	private List<Assertion> assertions;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public int getId() {
		return id;
	}

	public void setId(int id) {
		this.id = id;
	}

	public int getResourceId() {
		return resourceId;
	}

	public void setResourceId(int resourceId) {
		this.resourceId = resourceId;
	}

	public Map<String, String> getInput() {
		return input;
	}

	public void setInput(Map<String, String> input) {
		this.input = input;
	}

	public String getReferenceInput() {
		return referenceInput;
	}

	public void setReferenceInput(String referenceInput) {
		this.referenceInput = referenceInput;
	}

	public List<Assertion> getAssertions() {
		return assertions;
	}

	public void setAssertions(List<Assertion> assertions) {
		this.assertions = assertions;
	}
	
	
}
