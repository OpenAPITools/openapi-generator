package org.openapitools.virtualan.model;

import java.util.List;
import java.util.Set;

public class MockRequest {
	String input;
	Set excludeSet;
	List<MockKeyValue> availableParams;

	public List<MockKeyValue> getAvailableParams() {
		return availableParams;
	}

	public void setAvailableParams(List<MockKeyValue> availableParams) {
		this.availableParams = availableParams;
	}

	public String getInput() {
		return input;
	}

	public void setInput(String input) {
		this.input = input;
	}

	public Set getExcludeSet() {
		return excludeSet;
	}

	public void setExcludeSet(Set excludeSet) {
		this.excludeSet = excludeSet;
	}

	public MockRequest(String input, Set excludeSet, List<MockKeyValue> availableParams) {
		this.input = input;
		this.excludeSet = excludeSet;
		this.availableParams = availableParams;
	}

	@Override
	public String toString() {
		return "MockRequest [input=" + input + ", excludeSet=" + excludeSet + "]";
	}
}
