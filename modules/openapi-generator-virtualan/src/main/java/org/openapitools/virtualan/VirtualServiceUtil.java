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

package org.openapitools.virtualan;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.openapitools.virtualan.entity.VirtualServiceEntity;
import org.openapitools.virtualan.model.VirtualServiceRequest;
import org.openapitools.virtualan.model.VirtualServiceKeyValue;
import org.openapitools.virtualan.model.MockRequest;
import org.openapitools.virtualan.model.MockResponse;
import org.openapitools.virtualan.model.MockServiceRequest;
import org.openapitools.virtualan.model.VirtualServiceApiResponse;
import org.openapitools.virtualan.requestbody.RequestBody;
import org.openapitools.virtualan.requestbody.RequestBodyTypes;
import org.openapitools.virtualan.service.VirtualServiceService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.MessageSource;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;

@Service("virtualServiceUtil")
public class VirtualServiceUtil {

	private static final String PARAM_DELIMITER = ":_:";

	private static final Logger log = LoggerFactory.getLogger(VirtualServiceUtil.class);

	@Autowired
	private VirtualServiceService virtualService;

	@Autowired
	private MessageSource messageSource;

	Locale locale = LocaleContextHolder.getLocale();
	
	@Autowired
	private ObjectMapper objectMapper;

	@Autowired
	VirtualServiceInfo virtualServiceInfo; // Service which will do all data

	private ObjectMapper getObjectMapper() {
		objectMapper.findAndRegisterModules();
		return objectMapper.enable(DeserializationFeature.FAIL_ON_INVALID_SUBTYPE,
				DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES
		// ,DeserializationFeature.FAIL_ON_IGNORED_PROPERTIES
		);
	}

	public Map<String, String> getHttpStatusMap() {
		Map<String, String> map = new LinkedHashMap<>();
		for (HttpStatus status : HttpStatus.values()) {
			map.put(String.valueOf(status.value()), status.name());
		}
		return map;
	}

	public Map<MockRequest, MockResponse> readDynamicResponse(String resource, String operationId) throws IOException {

		Map<MockRequest, MockResponse> mockResponseMap = new HashMap();
		try {
			List<VirtualServiceRequest> mockTransferObjectList = virtualService.readByOperationId(resource, operationId);
			for (VirtualServiceRequest mockTransferObject : mockTransferObjectList) {
				String input = mockTransferObject.getInput();
				String output = mockTransferObject.getOutput();

				Set excludeSet = null;
				if (mockTransferObject.getExcludeList() != null) {
					excludeSet = new HashSet<String>(Arrays.asList(mockTransferObject.getExcludeList().split(",")));
				}
				MockRequest mockRequest = new MockRequest(input, excludeSet, mockTransferObject.getAvailableParams());
				MockResponse mockResponse = new MockResponse(output, mockTransferObject.getHttpStatusCode());
				mockResponseMap.put(mockRequest, mockResponse);
			}
		} catch (Exception e) {
			log.error("Rest Mock API Response for " + operationId + " has not loaded : " + e.getMessage());
		}
		return mockResponseMap;
	}

	public boolean compareQueryParams(MockRequest mockRequest, Map<String, String> actualQueryMap) {
		if (mockRequest.getAvailableParams() == null || mockRequest.getAvailableParams().isEmpty()) {
			return isEmptyRequest(actualQueryMap);
		} else {
			return isParameterMatch(mockRequest, actualQueryMap);
		}
	}

	private boolean isParameterMatch(MockRequest mockRequest, Map<String, String> actualQueryMap) {
		for (VirtualServiceKeyValue mockKeyValueParams : mockRequest.getAvailableParams()) {
			if (mockRequest.getExcludeSet() == null
					|| !mockRequest.getExcludeSet().contains(mockKeyValueParams.getKey())) {
				if (!mockKeyValueParams.getValue().equals(actualQueryMap.get(mockKeyValueParams.getKey()))) {
					return false;
				}
			}
		}
		return true;
	}

	private boolean isEmptyRequest(Map<String, String> actualQueryMap) {
		for (Map.Entry<String, String> checkEmpty : actualQueryMap.entrySet()) {
			if (!"null".equals(checkEmpty.getValue())) {
				return false;
			}
		}
		return true;
	}

	public boolean isMockAlreadyExists(VirtualServiceRequest mockTransferObject) {

		boolean isValid = false;
		try {
			Class inputObjectType = virtualServiceInfo.getInputType(mockTransferObject);

			HashMap<String, String> availableParamMap = new HashMap<>();
			if (mockTransferObject.getAvailableParams() != null && mockTransferObject.getAvailableParams().size() > 0) {
				for (VirtualServiceKeyValue availableParam : mockTransferObject.getAvailableParams()) {
					availableParamMap.put(availableParam.getKey(), availableParam.getValue());
				}
			}

			Map<MockRequest, MockResponse> mockDataSetupMap = readDynamicResponse(mockTransferObject.getResource(),
					mockTransferObject.getOperationId());
			for (Map.Entry<MockRequest, MockResponse> mockRequestResponse : mockDataSetupMap.entrySet()) {
				RequestBody requestBody = new RequestBody();
				requestBody.setObjectMapper(getObjectMapper());
				requestBody.setExcludeList(mockRequestResponse.getKey().getExcludeSet());
				requestBody.setExpectedInput(mockRequestResponse.getKey().getInput());
				requestBody.setInputObjectType(inputObjectType);
				requestBody.setInputRequest(mockTransferObject.getInput());
				if(inputObjectType != null) {
					requestBody.setActualInput(RequestBodyTypes.fromString(inputObjectType.getTypeName()).getValidMockRequestBody(requestBody));
				}
				if (availableParamMap != null && availableParamMap.size() > 0
						&& mockTransferObject.getInput() != null) {
					if (compareQueryParams(mockRequestResponse.getKey(), availableParamMap)
						&& RequestBodyTypes.fromString(inputObjectType.getTypeName()).compareRequestBody(requestBody)) {
						return true;
					}
				} else if (availableParamMap != null && availableParamMap.size() > 0
						&& compareQueryParams(mockRequestResponse.getKey(), availableParamMap)) {
					return true;
				} else if (mockTransferObject.getInput() != null 
						&& RequestBodyTypes.fromString(inputObjectType.getTypeName()).compareRequestBody(requestBody)){
					return true;
				}
			}
		} catch (Exception e) {
			log.error("isMockAlreadyExists :: " + e.getMessage());
		}
		return isValid;
	}

	public boolean isMockResponseBodyValid(VirtualServiceRequest mockTransferObject) {
		boolean isValid = true;
		try {
			VirtualServiceRequest mockTransferObjectActual = virtualServiceInfo.getResponseType(mockTransferObject);
			if (!mockTransferObjectActual.getResponseType().isEmpty()
					&& validResponse(mockTransferObjectActual, mockTransferObject)) {
				isValid = true;
			} else {
				isValid = false;
			}
		} catch (Exception e) {
			e.printStackTrace();
			isValid = false;
		}
		return isValid;
	}

	private boolean validResponse(VirtualServiceRequest mockTransferObjectActual, VirtualServiceRequest mockTransferObject)
			throws JsonParseException, JsonMappingException, ClassNotFoundException, IOException {
		VirtualServiceApiResponse apiResponse = mockTransferObjectActual.getResponseType()
				.get(mockTransferObject.getHttpStatusCode());
		if (apiResponse != null && apiResponse.getObjectType() != null) {
			try{
				objectMapper.readValue(mockTransferObject.getOutput(), Class.forName(apiResponse.getObjectType()));
			}catch(Exception e){
				
			}
		}
		return true;
	}

	

	public ResponseEntity returnResponse(MockServiceRequest mockServiceRequest)
			throws IOException {
		Map<MockRequest, MockResponse> mockDataSetupMap = readDynamicResponse(mockServiceRequest.getResource(),
				mockServiceRequest.getOperationId());
		for (Map.Entry<MockRequest, MockResponse> mockRequestResponse : mockDataSetupMap.entrySet()) {
			RequestBody requestBody = new RequestBody();
			requestBody.setObjectMapper(getObjectMapper());
			requestBody.setExcludeList(mockRequestResponse.getKey().getExcludeSet());
			requestBody.setExpectedInput(mockRequestResponse.getKey().getInput());
			requestBody.setInputObjectType(mockServiceRequest.getInputObjectType());
			requestBody.setActualInput(mockServiceRequest.getInputObject());
			ResponseEntity responseEntity = retrieveValidResponse(mockServiceRequest, mockRequestResponse, requestBody);
			if(responseEntity != null){
				return responseEntity; 
			}
		}
		return mockResponseNotFoundorSet(mockDataSetupMap);
	}

	private ResponseEntity mockResponseNotFoundorSet(Map<MockRequest, MockResponse> mockDataSetupMap) {
		if (mockDataSetupMap.size() > 0) {
			return new ResponseEntity(
					messageSource.getMessage("VS_RESPONSE_NOT_FOUND", null, locale),
					HttpStatus.INTERNAL_SERVER_ERROR);
		} else {
			log.error("Mock Response was not defined for the given input");
			return new ResponseEntity(
					messageSource.getMessage("VS_DATA_NOT_SET", null, locale),
					HttpStatus.INTERNAL_SERVER_ERROR);
		}
	}

	private ResponseEntity retrieveValidResponse(MockServiceRequest mockServiceRequest,
			Map.Entry<MockRequest, MockResponse> mockRequestResponse, RequestBody requestBody) throws IOException {
		if (mockServiceRequest.getParams() != null && mockServiceRequest.getParams().size() > 0
				&& mockServiceRequest.getInputObjectType() != null && mockServiceRequest.getInputObject() != null) {
			if (compareQueryParams(mockRequestResponse.getKey(), mockServiceRequest.getParams())
					&& RequestBodyTypes.fromString(mockServiceRequest.getInputObjectType().getTypeName()).compareRequestBody(requestBody)) {
				return new ResponseEntity(mockRequestResponse.getValue().getOutput(),
						HttpStatus.valueOf(Integer.parseInt(mockRequestResponse.getValue().getHttpStatusCode())));
			}
		} else if (mockServiceRequest.getParams() != null && mockServiceRequest.getParams().size() > 0) {
			if (compareQueryParams(mockRequestResponse.getKey(), mockServiceRequest.getParams())) {
				return new ResponseEntity(mockRequestResponse.getValue().getOutput(),
						HttpStatus.valueOf(Integer.parseInt(mockRequestResponse.getValue().getHttpStatusCode())));
			}
		} else if (mockServiceRequest.getInputObjectType() != null && 
					RequestBodyTypes.fromString(
					mockServiceRequest.getInputObjectType().getTypeName())
					.compareRequestBody(requestBody)){ 
				return new ResponseEntity(mockRequestResponse.getValue().getOutput(),
							HttpStatus.valueOf(Integer.parseInt(mockRequestResponse.getValue().getHttpStatusCode())));
		}
		return null;
	}

	public VirtualServiceRequest converterEToR(VirtualServiceEntity mockEntity) {
		VirtualServiceRequest request = new VirtualServiceRequest();
		BeanUtils.copyProperties(mockEntity, request);
		List<VirtualServiceKeyValue> availableParams = new LinkedList<>();
		if (mockEntity.getAvailableParamsList() != null) {
			String[] availableParamsList = mockEntity.getAvailableParamsList().split(PARAM_DELIMITER);
			if (availableParamsList != null && availableParamsList.length > 0) {
				for (String availableParamsStr : availableParamsList) {
					if (availableParamsStr.split("=").length == 2) {
						availableParams.add(
								new VirtualServiceKeyValue(availableParamsStr.split("=")[0], availableParamsStr.split("=")[1]));
					}
				}
			}
		}
		request.setAvailableParams(availableParams);
		return request;
	}

	public VirtualServiceEntity converterRToE(VirtualServiceRequest mockRequest) {
		VirtualServiceEntity mockEntity = new VirtualServiceEntity();
		BeanUtils.copyProperties(mockRequest, mockEntity);
		StringBuffer availableParamList = new StringBuffer();
		if (mockRequest.getAvailableParams() != null && mockRequest.getAvailableParams().size() > 0) {
			for (VirtualServiceKeyValue availableParam : mockRequest.getAvailableParams()) {
				if (availableParam.getValue() != null) {
					availableParamList.append(availableParam.getKey() + "=" + availableParam.getValue() + PARAM_DELIMITER);
				}
			}
			String availableParamStr = availableParamList.toString();
			if (availableParamStr.lastIndexOf(PARAM_DELIMITER) > 0) {
				mockEntity.setAvailableParamsList(availableParamStr.substring(0, availableParamStr.lastIndexOf(PARAM_DELIMITER)));
			} else if (availableParamStr != null && availableParamStr.trim().length() > 0) {
				mockEntity.setAvailableParamsList(availableParamStr);
			}
		}
		return mockEntity;
	}

}
