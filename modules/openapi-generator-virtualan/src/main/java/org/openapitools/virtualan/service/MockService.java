package org.openapitools.virtualan.service;

import java.util.List;

import org.openapitools.virtualan.model.Mock;

public interface MockService {

	Mock findById(long id);

	List<Mock> readByOperationId(String resource, String name);

	Mock saveMockRequest(Mock mockRequest);

	void updateMockRequest(Mock mockRequest);

	void deleteMockRequestById(long id);

	List<Mock> findAllMockRequests();

	boolean isMockRequestExist(Mock mockRequest);

}