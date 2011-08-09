/**
 *  Copyright 2011 Wordnik, Inc.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

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
