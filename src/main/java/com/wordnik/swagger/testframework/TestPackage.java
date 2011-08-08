package com.wordnik.swagger.testframework;

import java.util.List;

public class TestPackage {
	
	private List<TestResource> resources;
	
	private List<TestSuite> testSuites;

	
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
