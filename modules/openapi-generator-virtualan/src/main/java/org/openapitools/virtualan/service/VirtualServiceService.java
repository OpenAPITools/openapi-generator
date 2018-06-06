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

import java.util.List;

import org.openapitools.virtualan.model.VirtualServiceRequest;

public interface VirtualServiceService {

	VirtualServiceRequest findById(long id);

	List<VirtualServiceRequest> readByOperationId(String resource, String name);

	VirtualServiceRequest saveMockRequest(VirtualServiceRequest mockRequest);

	void updateMockRequest(VirtualServiceRequest mockRequest);

	void deleteMockRequestById(long id);

	List<VirtualServiceRequest> findAllMockRequests();

	boolean isMockRequestExist(VirtualServiceRequest mockRequest);

}