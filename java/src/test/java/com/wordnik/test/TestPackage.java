package com.wordnik.test;

import java.util.List;

public class TestPackage {
	
	private int id;

	private List<TestResource> resources;
	
	private List<TestSuite> testSuites;

	
	public int getId() {
		return id;
	}

	public void setId(int id) {
		this.id = id;
	}

	public List<TestResource> getResources() {
		return resources;
	}

	public void setResources(List<TestResource> resources) {
		this.resources = resources;
	}

	public List<TestSuite> getTestSuites() {
		return testSuites;
	}

	public void setTestSuites(List<TestSuite> testSuites) {
		this.testSuites = testSuites;
	}
	
	public TestCase getTestCaseById(int suiteId, int caseId) {
        TestSuite aSuite = null;
        TestCase aTestCase = null;
        if(this.getTestSuites() != null){
            for(TestSuite suite : getTestSuites()){
                if(suite.getId() == suiteId){
                    aSuite = suite;
                    break;
                }
            }
        }
        if(aSuite != null){
            for(TestCase testCase : aSuite.getTestCases()){
                if(testCase.getId() == caseId){
                    aTestCase = testCase;
                    break;
                }
            }
        }
        return aTestCase;
    }
}
