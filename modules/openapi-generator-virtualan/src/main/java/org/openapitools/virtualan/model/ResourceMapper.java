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

import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import org.openapitools.virtualan.VirtualServiceInfo;

public class ResourceMapper {
	String resource;
	String actualResource;
	Set<ResourceMapper> resourceMapperList = new LinkedHashSet<>();
	Map<String, String> operationIdMap = new HashMap<>();

	public ResourceMapper(String resource, Set<ResourceMapper> resourceMapperList) {
		this.resource = resource;
		this.resourceMapperList = resourceMapperList;
	}

	public ResourceMapper(String resource) {
		this.resource = resource;
	}

	public Map<String, String> getOperationIdMap() {
		return operationIdMap;
	}

	public void setOperationIdMap(Map<String, String> operationIdMap) {
		this.operationIdMap = operationIdMap;
	}

	public void setResourceMapperList(Set<ResourceMapper> resourceMapperList) {
		this.resourceMapperList = resourceMapperList;
	}

	@Override
	public String toString() {
		return "ResourceMapper [resource=" + resource + ", actualResource=" + actualResource
				+ ", resourceMapperList=" + resourceMapperList + ", operationId=" + operationIdMap + "]";
	}

	public ResourceMapper findResource(String resource) {
		if (resourceMapperList != null) {
			for (ResourceMapper mapper : resourceMapperList) {
				if (mapper.getResource().equalsIgnoreCase(resource)) {
					return mapper;
				}
			}
		}
		return null;
	}

	public void addResourceMapper(ResourceMapper resourceMapper) {
		if (resourceMapperList != null && resourceMapper != null) {
			resourceMapperList.add(resourceMapper);
		}
	}

	public String getActualResource() {
		return actualResource;
	}

	public void setActualResource(String actualResource) {
		this.actualResource = actualResource;
	}

	public String getResource() {
		return resource;
	}

	public void setResource(String resource) {
		this.resource = resource;
	}

	public Set<ResourceMapper> getResourceMapperList() {
		return resourceMapperList;
	}

	public void setResourceMapper(Set<ResourceMapper> resourceMapperList) {
		this.resourceMapperList = resourceMapperList;
	}

	public String getOperationId(String httpVerb) {
		return operationIdMap.get(httpVerb);
	}

	public void setOperationId(String httpVerb, String operationId) {
		operationIdMap.put(httpVerb, operationId);
		;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((actualResource == null) ? 0 : actualResource.hashCode());
		result = prime * result + ((operationIdMap == null) ? 0 : operationIdMap.hashCode());
		result = prime * result + ((resource == null) ? 0 : resource.hashCode());
		result = prime * result + ((resourceMapperList == null) ? 0 : resourceMapperList.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		ResourceMapper other = (ResourceMapper) obj;
		if (actualResource == null) {
			if (other.actualResource != null)
				return false;
		} else if (!actualResource.equals(other.actualResource))
			return false;
		if (operationIdMap == null) {
			if (other.operationIdMap != null)
				return false;
		} else if (!operationIdMap.equals(other.operationIdMap))
			return false;
		if (resource == null) {
			if (other.resource != null)
				return false;
		} else if (!resource.equals(other.resource))
			return false;
		if (resourceMapperList == null) {
			if (other.resourceMapperList != null)
				return false;
		} else if (!resourceMapperList.equals(other.resourceMapperList))
			return false;
		return true;
	}
}