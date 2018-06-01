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

	/*
	 * @CacheEvict(value = "mock", key =
	 * "{ #mockTransferObject.resource+'_'+#mockTransferObject.operationId}")
	 * public void removeCache(MockTransferObject mockTransferObject) {
	 * System.out.println("Cache Removed: " + mockTransferObject.getResource() +
	 * " : "+ mockTransferObject.getOperationId()); }
	 */

	// @CacheEvict(value = "mock", key = "{
	// #mockTransferObject.resource+'_'+#mockTransferObject.operationId}")
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

	/*
	 * public interface DomainOperations<T> { default List<T>
	 * filter(Predicate<T> predicate) { return persons.stream().filter(
	 * predicate ) .collect(Collectors.<Person>toList()); } }
	 */

	@Override
	@Transactional
	// @Cacheable(value = "mock", key = "{ #resource+'_'+#operationId}",
	// unless="#result==null")
	public List<Mock> readByOperationId(String resource, String operationId) {
		long startTime = System.currentTimeMillis();
		/*
		 * Iterable<MockEntity> mockTransferObjectList =
		 * mockRepository.findByOperationIdAndResource(operationId, resource);
		 * long endTime = System.currentTimeMillis(); System.out.println(
		 * "DB Call   " + (endTime-startTime)/1000); startTime =
		 * System.currentTimeMillis(); List<MockTransferObject> requestList =
		 * new ArrayList<>(); for (MockEntity mockEntity :
		 * mockTransferObjectList) { if
		 * (mockEntity.getResource().equals(resource) &&
		 * mockEntity.getOperationId().equals(operationId)) {
		 * requestList.add(converterEToR(mockEntity)); } }
		 */
		List<Mock> requestList = findAllMockRequests().stream()
				.filter(filterOperationIdAndResource(resource, operationId))
				.collect(Collectors.<Mock> toList());
		long endTime = System.currentTimeMillis();
		System.out.println("converter Calls   " + (endTime - startTime) / 1000);
		return requestList;
	}

}