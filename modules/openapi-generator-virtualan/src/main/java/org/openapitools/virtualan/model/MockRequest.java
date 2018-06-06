/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.virtualan.model;

import java.util.List;
import java.util.Set;

public class MockRequest {
	String input;
	Set excludeSet;
	List<VirtualServiceKeyValue> availableParams;

	public List<VirtualServiceKeyValue> getAvailableParams() {
		return availableParams;
	}

	public void setAvailableParams(List<VirtualServiceKeyValue> availableParams) {
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

	public MockRequest(String input, Set excludeSet, List<VirtualServiceKeyValue> availableParams) {
		this.input = input;
		this.excludeSet = excludeSet;
		this.availableParams = availableParams;
	}

	@Override
	public String toString() {
		return "MockRequest [input=" + input + ", excludeSet=" + excludeSet + "]";
	}
}
