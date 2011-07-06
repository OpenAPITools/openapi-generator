package com.wordnik.test;

import java.util.List;

/**
 * Test suite contains collection of test cases.
 * 
 * As  a best practice write one test suite for each resource
 * @author ramesh
 *
 */
public class TestSuite {
	
	private String name ;
	
	private int id;

	private List<TestCase> testCases;

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

	public List<TestCase> getTestCases() {
		return testCases;
	}

	public void setTestCases(List<TestCase> testCases) {
		this.testCases = testCases;
	}

	
}
