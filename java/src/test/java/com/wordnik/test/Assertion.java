package com.wordnik.test;

public class Assertion {

	private String expectedOutput;

    private String actualOutputWithOutputWrapper;

	private String condition;
	
	private String actualOutput;

	public String getExpectedOutput() {
		return expectedOutput;
	}

	public void setExpectedOutput(String expression) {
		this.expectedOutput = expression;
	}

	public String getCondition() {
		return condition;
	}

	public void setCondition(String condition) {
		this.condition = condition;
	}

	public String getActualOutput() {
		return actualOutput;
	}

	public void setActualOutput(String value) {
		this.actualOutput = value;
	}

    public String getActualOutputWithOutputWrapper() {
        return actualOutputWithOutputWrapper;
    }

    public void setActualOutputWithOutputWrapper(String actualOutputWithOutputWrapper) {
        this.actualOutputWithOutputWrapper = actualOutputWithOutputWrapper;
    }
}
