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

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Stores the status of the tests executed. 
 * @author ramesh
 *
 */
public class TestStatus {

	private int totalTestCaseCount;
	
	private int failedTestCaseCount;
	
	private List<TestCaseStatus> testCaseStatuses = new ArrayList<TestCaseStatus>();
	private Map<String, TestCaseStatus> testCaseStatusMap = new HashMap<String, TestCaseStatus>();
	
	public void logStatus(TestCase testCase, String testCasePath, boolean passed){
		logStatus(testCase, testCasePath, passed, null, null);	
	}

	public void logStatus(TestCase testCase, String testCasePath, boolean passed, String failureDescription){
		logStatus(testCase, testCasePath, passed, failureDescription, null);	
	}

	public void logStatus(TestCase testCase, String testCasePath, boolean passed, String failureDescription, Throwable exception){
		TestCaseStatus status = new TestCaseStatus(testCase, testCasePath, passed, failureDescription, exception);
		testCaseStatuses.add(status);
		testCaseStatusMap.put(testCasePath, status);
		totalTestCaseCount++;
		if(!passed){
			failedTestCaseCount++;
		}
		
	}

	public void logAssertionStatus(String path, boolean passed, String expectedValue, String actualValue, String condition) {
		logAssertionStatus(path, passed, expectedValue, actualValue, condition, null);
	}

	public void logAssertionStatus(String path, boolean passed, String expectedValue, String actualValue, String condition, Throwable stack) {
		TestCaseStatus status = testCaseStatusMap.get(path);
		if(!passed && status.passed) {
			status.passed = false;
			failedTestCaseCount++;
		}
		status.addAssertionStatus(passed, expectedValue, actualValue, condition, stack);
	}
	
	public String printTestStatus() {
		StringBuilder output = new StringBuilder();
		output.append("Summary -->  Total Test Cases: " + totalTestCaseCount + " Failed Test Cases: " + failedTestCaseCount);
		output.append("\nDetails: \n");
		for(TestCaseStatus status : testCaseStatuses) {
			output.append(status.getStatusDescription() + "\n");
		}
		return output.toString();
	}
	
	/**
	 * Indicates if there are any failured in executing the test cases. 
	 * @return
	 */
	public boolean hasFailures() {
		return (failedTestCaseCount > 0);
	}
	
	/**
	 * Inner class Stores the details on individual test cases.
	 * @author ramesh
	 *
	 */
	private class TestCaseStatus {
		
		String testCasePath;
		
		boolean passed;
		
		String failureDescription;
		
		Throwable exceptionStack;
		
		TestCase testCase;
		
		List<AssertionStatus> assertionStatuses = new ArrayList<AssertionStatus>();
		
		public TestCaseStatus(TestCase testCase, String testCasePath, boolean passed, String failureDescription, 
				Throwable exceptionStack) {
			this.exceptionStack = exceptionStack;
			this.testCase = testCase;
			this.passed = passed;
			this.failureDescription = failureDescription;
			this.testCasePath = testCasePath;
		}
		
		public void addAssertionStatus(boolean passed, String expectedValue, String actualValue, String condition, Throwable stack) {
			String assertionDesc  = expectedValue + " " + condition + " " + actualValue;
			AssertionStatus status = new AssertionStatus(assertionDesc, passed, stack);
			assertionStatuses.add(status);
			if(!passed){
				this.passed = false;
			}
		}
		
		public String getStatusDescription(){
			StringBuilder output = new StringBuilder();
			if(passed){
				output.append(testCasePath);
				output.append(" : ");
				output.append(testCase.getName());
				output.append(" : ");
				output.append(" passed ");
				output.append(" \n ");
			}else{
				output.append(testCasePath);
				output.append(" : ");
				output.append(testCase.getName());
				output.append(" : ");
				output.append(" failed ");
				output.append(" \n ");
				if ( failureDescription != null) {
					output.append(" failure message : ");
					output.append(failureDescription + "\n");
				}
				if(exceptionStack != null){
                    StringWriter out = new StringWriter();
                    PrintWriter writer = new PrintWriter(out);
                    exceptionStack.printStackTrace(writer);
					output.append(out.toString());
				}
				for(AssertionStatus status:assertionStatuses){
					if(!status.passed) {
						output.append(status.getAssertionDescription()+"\n");
					}
				}
			}
			return output.toString();
		}
	}
	
	private class AssertionStatus {
		
		String assertionDescription;
		
		boolean passed;
		
		Throwable exceptionStack;
		
		public AssertionStatus(String description, boolean passed, Throwable stack){
			this.assertionDescription = description;
			this.passed = passed;
			this.exceptionStack = stack;
		}
		
		public String getAssertionDescription() {
			return assertionDescription + " : " + (passed ? "Passed " : " Failed ") + ((exceptionStack !=null) ? exceptionStack.getMessage() : "" );
		}
	}
}
