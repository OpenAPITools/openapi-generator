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

package com.wordnik.swagger.codegen.resource;


import java.util.List;

/**
 * User: ramesh
 * Date: 3/31/11
 * Time: 7:55 AM
 */
public class Response {

    private String valueType;

    private String condition;

    private List<ErrorResponse> errorResponses;

	public String getValueType() {
		return valueType;
	}

	public void setValueType(String valueType) {
		this.valueType = valueType;
	}

	public String getCondition() {
		return condition;
	}

	public void setCondition(String condition) {
		this.condition = condition;
	}

	public List<ErrorResponse> getErrorResponses() {
		return errorResponses;
	}

	public void setErrorResponses(List<ErrorResponse> errorResponses) {
		this.errorResponses = errorResponses;
	}

    
}
