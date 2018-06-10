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

package org.openapitools.virtualan.entity;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.SequenceGenerator;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "virtual_service")
public class VirtualServiceEntity implements Serializable {

	private static final long serialVersionUID = -1801714432822866390L;

	@Id
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "virtual_service_generator")
	@SequenceGenerator(name="virtual_service_generator", sequenceName = "virtual_service_seq")
	@Column(name = "id", updatable = false, nullable = false)
	private long id;

	@Column(name = "operationId", nullable = false)
	private String operationId;

	@Column(name = "input")
	private String input;

	@Column(name = "output")
	private String output;

	@Column(name = "httpStatusCode")
	private String httpStatusCode;

	@Column(name = "resources")
	private String resource;
	
	@Column(name = "url")
	private String url;
	
	@Column(name = "method")
	private String method;
	
	@Column(name = "excludeList")
	private String excludeList;

	@Column(name = "availableParamsList")
	private String availableParamsList;

	public String getAvailableParamsList() {
		return availableParamsList;
	}

	public void setAvailableParamsList(String availableParamsList) {
		this.availableParamsList = availableParamsList;
	}

	public VirtualServiceEntity() {

	}

	public VirtualServiceEntity(String operationId, String input, String output) {
		this.operationId = operationId;
		this.input = input;
		this.output = output;
		this.httpStatusCode = httpStatusCode;
	}

	public String getResource() {
		return resource;
	}

	public void setResource(String resource) {
		this.resource = resource;
	}

	public String getExcludeList() {
		return excludeList;
	}

	public void setExcludeList(String excludeList) {
		this.excludeList = excludeList;
	}

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

	public String getOperationId() {
		return operationId;
	}

	public void setOperationId(String operationId) {
		this.operationId = operationId;
	}

	public String getInput() {
		return input;
	}

	public void setInput(String input) {
		this.input = input;
	}

	public String getOutput() {
		return output;
	}

	public void setOutput(String output) {
		this.output = output;
	}

	public String getHttpStatusCode() {
		return httpStatusCode;
	}

	public void setHttpStatusCode(String httpStatusCode) {
		this.httpStatusCode = httpStatusCode;
	}

	public String getUrl() {
		return url;
	}

	public void setUrl(String url) {
		this.url = url;
	}

	public String getMethod() {
		return method;
	}

	public void setMethod(String method) {
		this.method = method;
	}

}