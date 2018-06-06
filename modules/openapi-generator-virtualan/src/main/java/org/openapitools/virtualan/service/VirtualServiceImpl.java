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

package org.openapitools.virtualan.service;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import org.openapitools.virtualan.VirtualServiceUtil;
import org.openapitools.virtualan.dao.VirtualServiceRepository;
import org.openapitools.virtualan.model.Cache;
import org.openapitools.virtualan.model.VirtualServiceRequest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service("virtualService")
public class VirtualServiceImpl implements VirtualServiceService {

	@Autowired
	private VirtualServiceRepository virtualServiceRepository;

	
	@Autowired
	private VirtualServiceUtil virtualServiceUtil; 
	
	
	@Autowired
	@Qualifier("virtualCache")
	private Cache<Long, VirtualServiceRequest> cache;
	

	@Transactional
	public List<VirtualServiceRequest> findAllMockRequests() {
		return new ArrayList<VirtualServiceRequest>(cache.readAll().values());
	}

	@Transactional
	public VirtualServiceRequest findById(long id) {
		return cache.readById(id);
	}

	
	public VirtualServiceRequest saveMockRequest(VirtualServiceRequest mockTransferObject) {
		VirtualServiceRequest mockTransferObjectResponse = virtualServiceUtil.converterEToR(
				virtualServiceRepository.save(virtualServiceUtil.converterRToE(mockTransferObject)));
		cache.update(mockTransferObjectResponse.getId(), mockTransferObjectResponse);
		return mockTransferObjectResponse;
	}

	public void updateMockRequest(VirtualServiceRequest mockRequest) {
		VirtualServiceRequest mockTransferObjectResponse = virtualServiceUtil.converterEToR(virtualServiceRepository.save(virtualServiceUtil.converterRToE(mockRequest)));
		cache.update(mockTransferObjectResponse.getId(), mockTransferObjectResponse);
	}

	public void deleteMockRequestById(long id) {
		VirtualServiceRequest mockTransferObject = findById(id);
		virtualServiceRepository.deleteById(id);
		cache.remove(id);
	}

	public boolean isMockRequestExist(VirtualServiceRequest mockRequest) {
		return cache.readById(mockRequest.getId()) != null;
	}

	
	public static Predicate<VirtualServiceRequest> filterOperationIdAndResource(String resource, String operationId) {
		return p -> (p.getResource().equalsIgnoreCase(resource) && p.getOperationId().equalsIgnoreCase(operationId));
	}

	@Override
	@Transactional
	public List<VirtualServiceRequest> readByOperationId(String resource, String operationId) {
		List<VirtualServiceRequest> requestList = findAllMockRequests().stream()
				.filter(filterOperationIdAndResource(resource, operationId))
				.collect(Collectors.<VirtualServiceRequest> toList());
		return requestList;
	}

}