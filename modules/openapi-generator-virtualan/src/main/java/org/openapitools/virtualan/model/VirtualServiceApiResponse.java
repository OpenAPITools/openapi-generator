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

public class VirtualServiceApiResponse {

	private String code;
	private String objectType;
	private String objectValue;
	private String objectMessage;

	public VirtualServiceApiResponse() {

	}

	public VirtualServiceApiResponse(String code, String objectType, String objectValue, String objectMessage) {
		super();
		this.code = code;
		this.objectType = objectType;
		this.objectValue = objectValue;
		this.objectMessage = objectMessage;
	}

	public String getCode() {
		return code;
	}

	public void setCode(String code) {
		this.code = code;
	}

	public String getObjectType() {
		return objectType;
	}

	public void setObjectType(String objectType) {
		this.objectType = objectType;
	}

	public String getObjectValue() {
		return objectValue;
	}

	public void setObjectValue(String objectValue) {
		this.objectValue = objectValue;
	}

	public String getObjectMessage() {
		return objectMessage;
	}

	public void setObjectMessage(String objectMessage) {
		this.objectMessage = objectMessage;
	}

}
