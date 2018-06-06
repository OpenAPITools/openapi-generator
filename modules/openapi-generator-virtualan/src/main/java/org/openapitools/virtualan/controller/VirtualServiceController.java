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

package org.openapitools.virtualan.controller;

import java.io.IOException;
import java.lang.invoke.MethodHandles;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.annotation.PostConstruct;

import org.openapitools.virtualan.VirtualServiceInfo;
import org.openapitools.virtualan.VirtualServiceUtil;
import org.openapitools.virtualan.model.VirtualServiceRequest;
import org.openapitools.virtualan.model.VirtualServiceStatus;
import org.openapitools.virtualan.requestbody.RequestBodyTypes;
import org.openapitools.virtualan.service.VirtualServiceService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.MessageSource;
import org.springframework.context.NoSuchMessageException;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.core.io.Resource;
import org.springframework.core.io.support.PathMatchingResourcePatternResolver;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.mvc.method.annotation.RequestMappingHandlerMapping;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;

@RestController
public class VirtualServiceController {

	@Autowired
	private VirtualServiceService virtualService; 

	@Autowired
	private ObjectMapper objectMapper;


	private ObjectMapper getObjectMapper() {
		objectMapper.findAndRegisterModules();
		return objectMapper.enable(DeserializationFeature.FAIL_ON_INVALID_SUBTYPE,
				DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES
		// ,DeserializationFeature.FAIL_ON_IGNORED_PROPERTIES
		);
	}

	@Autowired
	VirtualServiceInfo virtualServiceInfo;

	@Autowired
	private MessageSource messageSource;

	Locale locale = LocaleContextHolder.getLocale();
	
	@Autowired
	private VirtualServiceUtil virtualServiceUtil;

	private final RequestMappingHandlerMapping handlerMapping;

	@Autowired
	public VirtualServiceController(RequestMappingHandlerMapping handlerMapping) {
		this.handlerMapping = handlerMapping;
	}
	
	@PostConstruct
	public void init() throws ClassNotFoundException, JsonProcessingException, InstantiationException, IllegalAccessException{
		virtualServiceInfo.loadVirtualServices(handlerMapping);
		virtualServiceInfo.loadMapper();
	}
	
	@RequestMapping(value = "/virtualservices/load", method = RequestMethod.GET)
	public Map<String, Map<String, VirtualServiceRequest>> listAllMockLoadRequest() throws InstantiationException,
			IllegalAccessException, ClassNotFoundException, JsonParseException, JsonMappingException, IOException {
		return virtualServiceInfo.loadVirtualServices(handlerMapping);
	}

	@RequestMapping(value = "/virtualservices", method = RequestMethod.GET)
	public ResponseEntity<List<VirtualServiceRequest>> listAllMockLoadRequests(){
		List<VirtualServiceRequest> MockLoadRequests = virtualService.findAllMockRequests(); 
		if (MockLoadRequests.isEmpty()) {
			return new ResponseEntity<List<VirtualServiceRequest>>(HttpStatus.NO_CONTENT);
		}
		return new ResponseEntity<List<VirtualServiceRequest>>(MockLoadRequests, HttpStatus.OK);
	}


	@RequestMapping(value = "/virtualservices/{id}", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<VirtualServiceRequest> getMockLoadRequest(@PathVariable("id") long id) {
		VirtualServiceRequest mockLoadRequest = virtualService.findById(id);
		if (mockLoadRequest == null) {
			return new ResponseEntity<VirtualServiceRequest>(HttpStatus.NOT_FOUND);
		}
		return new ResponseEntity<VirtualServiceRequest>(mockLoadRequest, HttpStatus.OK);
	}


	@RequestMapping(value = "/virtualservices", method = RequestMethod.POST)
	public ResponseEntity createMockRequest(@RequestBody VirtualServiceRequest virtualServiceRequest) {
		
		try {
			
			validateExpectedInput(virtualServiceRequest);

			//find the operationId for the given Request. It required for the Automation test cases
			findOperationIdForService(virtualServiceRequest);
			
			ResponseEntity responseEntity = validateRequestBody(virtualServiceRequest);
			if(responseEntity != null) {
				return responseEntity;
			}

			responseEntity = validateResponseBody(virtualServiceRequest);
			if(responseEntity != null) {
				return responseEntity;
			}

			responseEntity = checkIfServiceDataAlreadyExists(virtualServiceRequest);
			
			if(responseEntity != null) {
				return responseEntity;
			}
			
			VirtualServiceRequest mockTransferObject = virtualService.saveMockRequest(virtualServiceRequest);
			
			mockTransferObject.setMockStatus(new VirtualServiceStatus(messageSource.getMessage("VS_SUCCESS", null, locale)));
			return new ResponseEntity<>(mockTransferObject, HttpStatus.CREATED);
		
		} catch (Exception e) {
			return new ResponseEntity<VirtualServiceStatus>(new VirtualServiceStatus(messageSource.getMessage("VS_UNEXPECTED_ERROR", null, locale) + e.getMessage()),
					HttpStatus.BAD_REQUEST);
		}
	}

	private ResponseEntity checkIfServiceDataAlreadyExists(VirtualServiceRequest mockLoadRequest) {
		if (virtualServiceUtil.isMockAlreadyExists(mockLoadRequest)) {
			return new ResponseEntity<VirtualServiceStatus>(
					new VirtualServiceStatus(messageSource.getMessage("VS_DATA_ALREADY_EXISTS", null, locale)),
					HttpStatus.BAD_REQUEST);
		}
		return null;
	}

	private ResponseEntity validateResponseBody(VirtualServiceRequest mockLoadRequest) {
		if (!virtualServiceUtil.isMockResponseBodyValid(mockLoadRequest)) {
			return new ResponseEntity<VirtualServiceStatus>(
					new VirtualServiceStatus(messageSource.getMessage("VS_RESPONSE_BODY_MISMATCH", null, locale)),
					HttpStatus.BAD_REQUEST);
		}
		return null;
	}

	private ResponseEntity validateRequestBody(VirtualServiceRequest virtualServiceRequest) {
		Class inputObjectType = virtualServiceInfo.getInputType(virtualServiceRequest);
		if (inputObjectType == null
				&& (virtualServiceRequest.getInput() == null || virtualServiceRequest.getInput().length() == 0)) {
			return null;
		} else if (virtualServiceRequest.getInput() != null && virtualServiceRequest.getInput().length() > 0
				&& inputObjectType != null) {
			org.openapitools.virtualan.requestbody.RequestBody requestBody = new org.openapitools.virtualan.requestbody.RequestBody();
			requestBody.setObjectMapper(getObjectMapper());
			requestBody.setInputRequest(virtualServiceRequest.getInput());
			requestBody.setInputObjectType(inputObjectType);
			Object object =  null;
			try {
				object = RequestBodyTypes.fromString(inputObjectType.getTypeName()).getValidMockRequestBody(requestBody);
			} catch (NoSuchMessageException | IOException e) {
				object  = null;
			}
			if (object == null) {
				return new ResponseEntity<VirtualServiceStatus>(
					new VirtualServiceStatus(messageSource.getMessage("VS_REQUEST_BODY_MISMATCH", null, locale)),
					HttpStatus.BAD_REQUEST);
			}
		}
		return null;
	}

	private void findOperationIdForService(VirtualServiceRequest mockLoadRequest) {
		if(mockLoadRequest.getOperationId() == null) {
			String resourceUrl = mockLoadRequest.getUrl().substring(1, mockLoadRequest.getUrl().length());
			List<String> resouceSplitterList = new LinkedList(Arrays.asList(resourceUrl.split("/")));
			if(resouceSplitterList.size() >0) {
				String operationId = virtualServiceInfo.getOperationId( mockLoadRequest.getMethod(), virtualServiceInfo.getResourceParent(), resouceSplitterList);
				mockLoadRequest.setOperationId(operationId);
				mockLoadRequest.setResource(resouceSplitterList.get(0));
			}
		}
	}

	private ResponseEntity validateExpectedInput(VirtualServiceRequest mockLoadRequest) {
		if(mockLoadRequest.getHttpStatusCode() == null 
				|| mockLoadRequest.getMethod() == null 
				|| mockLoadRequest.getUrl() == null) {
			return new ResponseEntity<VirtualServiceStatus>(
					new VirtualServiceStatus(messageSource.getMessage("VS_CREATE_MISSING_INFO", null, locale)),
					HttpStatus.BAD_REQUEST);
		}
		return null; 
	}

	@RequestMapping(value = "/virtualservices/{id}", method = RequestMethod.PUT)
	public ResponseEntity<VirtualServiceRequest> updateMockRequest(@PathVariable("id") long id,
			@RequestBody VirtualServiceRequest mockLoadRequest) {

		VirtualServiceRequest currentMockLoadRequest = virtualService.findById(id);
		if (currentMockLoadRequest == null) {
			return new ResponseEntity<VirtualServiceRequest>(HttpStatus.NOT_FOUND);
		}

		currentMockLoadRequest.setInput(mockLoadRequest.getInput());
		currentMockLoadRequest.setOutput(mockLoadRequest.getOutput());
		currentMockLoadRequest.setOperationId(mockLoadRequest.getOperationId());

		virtualService.updateMockRequest(currentMockLoadRequest);
		return new ResponseEntity<VirtualServiceRequest>(currentMockLoadRequest, HttpStatus.OK);
	}


	@RequestMapping(value = "/virtualservices/{id}", method = RequestMethod.DELETE)
	public ResponseEntity<VirtualServiceRequest> deleteMockRequest(@PathVariable("id") long id) {
		VirtualServiceRequest MockLoadRequest = virtualService.findById(id);
		if (MockLoadRequest == null) {
			return new ResponseEntity<VirtualServiceRequest>(HttpStatus.NOT_FOUND);
		}
		virtualService.deleteMockRequestById(id);
		return new ResponseEntity<VirtualServiceRequest>(HttpStatus.NO_CONTENT);
	}
	
	
	@RequestMapping(value = "/swagger-catalogs/{name}", method = RequestMethod.GET)
	public ResponseEntity<List<String>> readCatalog(@PathVariable("name") String name) {
		List<String> fileList = new LinkedList<>();
		try {
			for (Resource file : getCatalogs(name)) {
			   fileList.add(file.getFilename());
			}
		} catch (IOException e) {
			return new ResponseEntity<List<String>>(HttpStatus.NOT_FOUND);
		}
		if (fileList.isEmpty()) {
			return new ResponseEntity<List<String>>(HttpStatus.NOT_FOUND);
		} else {
			return new ResponseEntity<List<String>>(fileList, HttpStatus.OK);
		}
	}
	
	
	private Resource[] getCatalogs(String name) throws IOException
	{
	    ClassLoader classLoader = MethodHandles.lookup().getClass().getClassLoader();
	    PathMatchingResourcePatternResolver resolver = new PathMatchingResourcePatternResolver(classLoader);
	    return resolver.getResources("classpath:META-INF/resources/yaml/"+name+"/*.yaml");
	}
	
}