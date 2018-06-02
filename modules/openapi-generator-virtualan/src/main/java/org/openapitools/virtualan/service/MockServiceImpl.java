package org.openapitools.virtualan.service;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import org.openapitools.virtualan.VirtualServiceUtil;
import org.openapitools.virtualan.dao.MockRepository;
import org.openapitools.virtualan.model.Cache;
import org.openapitools.virtualan.model.Mock;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service("mockService")
public class MockServiceImpl implements MockService {

	@Autowired
	private MockRepository mockRepository;

	
	@Autowired
	private VirtualServiceUtil virtualServiceUtil; 
	
	
	@Autowired
	@Qualifier("virtualCache")
	private Cache<Long, Mock> cache;
	

	@Transactional
	public List<Mock> findAllMockRequests() {
		return new ArrayList<Mock>(cache.readAll().values());
	}

	@Transactional
	public Mock findById(long id) {
		return cache.readById(id);
	}

	
	public Mock saveMockRequest(Mock mockTransferObject) {
		Mock mockTransferObjectResponse = virtualServiceUtil.converterEToR(
				mockRepository.save(virtualServiceUtil.converterRToE(mockTransferObject)));
		cache.update(mockTransferObjectResponse.getId(), mockTransferObjectResponse);
		return mockTransferObjectResponse;
	}

	public void updateMockRequest(Mock mockRequest) {
		Mock mockTransferObjectResponse = virtualServiceUtil.converterEToR(mockRepository.save(virtualServiceUtil.converterRToE(mockRequest)));
		cache.update(mockTransferObjectResponse.getId(), mockTransferObjectResponse);
	}

	public void deleteMockRequestById(long id) {
		Mock mockTransferObject = findById(id);
		mockRepository.deleteById(id);
		cache.remove(id);
	}

	public boolean isMockRequestExist(Mock mockRequest) {
		return cache.readById(mockRequest.getId()) != null;
	}

	

	public static Predicate<Mock> filterOperationIdAndResource(String resource, String operationId) {
		return p -> (p.getResource().equalsIgnoreCase(resource) && p.getOperationId().equalsIgnoreCase(operationId));
	}

	

	@Override
	@Transactional
	public List<Mock> readByOperationId(String resource, String operationId) {
		long startTime = System.currentTimeMillis();
		List<Mock> requestList = findAllMockRequests().stream()
				.filter(filterOperationIdAndResource(resource, operationId))
				.collect(Collectors.<Mock> toList());
		return requestList;
	}

}