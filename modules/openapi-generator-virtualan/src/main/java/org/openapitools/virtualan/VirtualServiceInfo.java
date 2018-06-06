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
import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.openapitools.virtualan.model.VirtualServiceRequest;
import org.openapitools.virtualan.model.VirtualServiceKeyValue;
import org.openapitools.virtualan.model.ResourceMapper;
import org.openapitools.virtualan.model.VirtualServiceApiResponse;
import org.openapitools.virtualan.requestbody.RequestBodyTypes;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.servlet.mvc.method.RequestMappingInfo;
import org.springframework.web.servlet.mvc.method.annotation.RequestMappingHandlerMapping;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

@Service("virtualServiceInfo")
public class VirtualServiceInfo {

	private static final String MATCH_API_CONTROLLER = "[class\\sa-zA-Z0-9]ApiController";

	private static final String API_CONTROLLER = "ApiController";

	private static final String API_SUFFIX = "Api";

	private static final String PARENT_ROOT = "Parent-Root";

	private static final String CURLY_PATH = "Curly";

	private static final Logger log = LoggerFactory.getLogger(VirtualServiceInfo.class);

	@Autowired
	private ObjectMapper objectMapper;

	final String rxpCurly = "\\{(.*?)\\}";
	final Pattern pattern = Pattern.compile(rxpCurly, Pattern.MULTILINE);
	ResourceMapper resourceParent;

	public ResourceMapper getResourceParent() {
		return resourceParent;
	}

	public void setResourceParent(ResourceMapper resourceParent) {
		this.resourceParent = resourceParent;
	}

	Map<String, Map<String, VirtualServiceRequest>> mockLoadChoice;

	public Map<String, Class> findVirtualServices(RequestMappingHandlerMapping handlerMapping) {
		Map<String, Class> virtualInterfaces = new HashMap<>();
		Pattern p = Pattern.compile(MATCH_API_CONTROLLER);
		for (Map.Entry<RequestMappingInfo, HandlerMethod> mapSwaggerAPIEntry : handlerMapping.getHandlerMethods()
				.entrySet()) {
			Matcher m = p.matcher(mapSwaggerAPIEntry.getValue().getBeanType().toString()); 
			if (m.find()) {
				String interfaceName = mapSwaggerAPIEntry.getValue().getBeanType().getName().replace(API_CONTROLLER, API_SUFFIX);
				try {
					Class intefaceController = Class.forName(interfaceName);
					interfaceName = interfaceName.substring(interfaceName.lastIndexOf(".") + 1, interfaceName.length());
 					interfaceName = interfaceName.toLowerCase();
					virtualInterfaces.put(interfaceName, intefaceController);
				} catch (Exception e) {
					log.info("Not a interfaceName for  Virtual API : " + interfaceName);
				}
			}
		}
		return virtualInterfaces;
	}

	private VirtualServiceRequest getResource(Map.Entry<String, Class> virtualServiceEntry) {
		VirtualServiceRequest Mock = new VirtualServiceRequest();
		Api[] apiOperationAnnos = (Api[]) virtualServiceEntry.getValue().getAnnotationsByType(Api.class);
		if (apiOperationAnnos != null) {
			for (Api api : apiOperationAnnos) {
				if (api.value() != null) {
					Mock.setResource(api.value());
				}
			}
		}
		return Mock;
	}

	public Map<String, Map<String, VirtualServiceRequest>> loadVirtualServices(RequestMappingHandlerMapping handlerMapping)
			throws ClassNotFoundException, JsonProcessingException, InstantiationException, IllegalAccessException {
		if (mockLoadChoice == null) {
			mockLoadChoice = new TreeMap<>();
			for (Map.Entry<String, Class> virtualServiceEntry : findVirtualServices(handlerMapping).entrySet()) {
				mockLoadChoice.put(virtualServiceEntry.getKey(), buildVirtualServiceInfo(virtualServiceEntry));
			}
		}
		return mockLoadChoice;
	}

	private Map<String, VirtualServiceRequest>  buildVirtualServiceInfo(Map.Entry<String, Class> virtualServiceEntry)
			throws JsonProcessingException, InstantiationException, IllegalAccessException, ClassNotFoundException {
		Map<String, VirtualServiceRequest>  mockAPILoadChoice = new LinkedHashMap<String, VirtualServiceRequest>();
		for (Method method : virtualServiceEntry.getValue().getDeclaredMethods()) {
			VirtualServiceRequest MockReturn  = buildServiceDetails(virtualServiceEntry, method);
			if(MockReturn != null){
				mockAPILoadChoice.put(method.getName(), MockReturn);
			}
		}
		return mockAPILoadChoice;
	}

	private VirtualServiceRequest buildServiceDetails(Map.Entry<String, Class> virtualServiceEntry, Method method)
			throws JsonProcessingException, InstantiationException, IllegalAccessException, ClassNotFoundException {
		RequestMapping[] annotInstance = method.getAnnotationsByType(RequestMapping.class);
		VirtualServiceRequest mockLoadRequest = getResource(virtualServiceEntry);
		mockLoadRequest .setDesc(getResourceDesc(method));
		if (annotInstance != null && annotInstance.length > 0) {
			RequestMapping requestMapping = ((RequestMapping) annotInstance[0]);
			if (requestMapping.value() != null && requestMapping.value().length > 0) {
				mockLoadRequest.setUrl(requestMapping.value()[0]);
			}
			if (requestMapping.method() != null && requestMapping.method().length > 0) {
				mockLoadRequest.setMethod(requestMapping.method()[0].name());
			}
			mockLoadRequest.setResponseType(buildResponseType(method));
			buildInput(method, mockLoadRequest);
			mockLoadRequest.setOperationId(method.getName());
			mockLoadRequest.setHttpStatusMap(getHttpStatusMap());
			return mockLoadRequest;
		}
		return null;
	}

	private String getResourceDesc(Method method) {
		ApiOperation[] apiOperationAnno = method.getAnnotationsByType(ApiOperation.class);
		if(apiOperationAnno != null && apiOperationAnno.length > 0){
			return apiOperationAnno[0].notes();
		} 
		return null; 
	}

	private void buildInput(Method method, VirtualServiceRequest mockLoadRequest)
			throws JsonProcessingException, InstantiationException, IllegalAccessException, ClassNotFoundException {
		int i = 0;
		List<VirtualServiceKeyValue> availableParams = new ArrayList();
		Annotation[][] annotations = method.getParameterAnnotations();
		Class[] parameterTypes = method.getParameterTypes();
		for (Annotation[] anns : annotations) {
			Class parameterType = parameterTypes[i++];
			for (Annotation paramAnnotation : anns) {
				if (paramAnnotation.annotationType().equals(RequestParam.class)) {
					RequestParam requestParam = (RequestParam) paramAnnotation;
					availableParams.add(new VirtualServiceKeyValue(requestParam.value(), null));
				} else if (paramAnnotation.annotationType().equals(PathVariable.class)) {
					PathVariable pathVariable = (PathVariable) paramAnnotation;
					availableParams.add(new VirtualServiceKeyValue(pathVariable.value(), null));
				} else if (paramAnnotation.annotationType().equals(RequestBody.class)) {
					org.openapitools.virtualan.requestbody.RequestBody requestBody = new org.openapitools.virtualan.requestbody.RequestBody();
					requestBody.setInputObjectTypeName(
							Class.forName(parameterType.getName()).getTypeName());
					requestBody.setInputObjectType(parameterType);
					requestBody.setObjectMapper(objectMapper);
					mockLoadRequest.setInputObjectType(Class.forName(parameterType.getName()));
					try {
						mockLoadRequest.setInput(RequestBodyTypes
								.fromString(requestBody.getInputObjectTypeName())
								.getDefaultMessageBody(requestBody));
					} catch (IOException e) {

						// TO-DO
					}
				}
			}
		}
		mockLoadRequest.setAvailableParams(availableParams);
	}

	private Map<String, VirtualServiceApiResponse> buildResponseType(Method method)
			throws JsonProcessingException, InstantiationException, IllegalAccessException, ClassNotFoundException {
		Map<String, VirtualServiceApiResponse> responseType = new HashMap<>();
		ApiResponses[] apiResponsesAnno = method.getAnnotationsByType(ApiResponses.class);
		if (apiResponsesAnno != null) {
			for (ApiResponses apiResponses : apiResponsesAnno) {
				for (ApiResponse apiResponse : apiResponses.value()) {
					try {
						responseType.put(String.valueOf(apiResponse.code()),
								new VirtualServiceApiResponse(
										String.valueOf(apiResponse.code()),
										apiResponse.response().getCanonicalName(),
										objectMapper.writerWithDefaultPrettyPrinter()
												.writeValueAsString(Class
														.forName(apiResponse.response()
																.getCanonicalName())
														.newInstance()),
										apiResponse.message()));
					} catch (Exception e) {
						responseType.put(String.valueOf(apiResponse.code()),
								new VirtualServiceApiResponse(
										String.valueOf(apiResponse.code()), null, null,
										apiResponse.message()));
					}
				}
			}

		}
		return responseType;
	}

	public Class getInputType(VirtualServiceRequest mockTransferInput) {
		Class inputType = null;
		if (mockTransferInput.getResource() != null) {
			String resource = (mockTransferInput.getResource().substring(0, 1).toUpperCase()
					+ mockTransferInput.getResource().substring(1) + API_SUFFIX).replaceAll("[^a-zA-Z0-9]", "").toLowerCase();
			if (mockTransferInput.getOperationId() != null) {
				VirtualServiceRequest mockTransferActual = mockLoadChoice.get(resource)
						.get(mockTransferInput.getOperationId());
				if (mockTransferActual != null) {
					inputType = mockTransferActual.getInputObjectType();
				}
			} else {
				String resourceUrl = mockTransferInput.getUrl().substring(1, mockTransferInput.getUrl().length());
				List<String> resouceSplitterList = new LinkedList(Arrays.asList(resourceUrl.split("/")));
				if (resouceSplitterList.size() > 0) {
					String operationId = getOperationId(mockTransferInput.getMethod(), resourceParent,
							resouceSplitterList);
					VirtualServiceRequest mockTransferActual = mockLoadChoice.get(resource).get(operationId);
					if (mockTransferActual != null) {
						inputType = mockTransferActual.getInputObjectType();
					}
				}
			}
		}
		return inputType;
	}

	public String getOperationId(String httpVerb, ResourceMapper resourceParent, List<String> resouceSplitterList) {
		if (resouceSplitterList.size() == 0) {
			return resourceParent.getOperationId(httpVerb);
		}
		String resource = resouceSplitterList.get(0);
		ResourceMapper mapper = resourceParent.findResource(resource);
		if (mapper != null) {
			return getOperationId(httpVerb, mapper, resouceSplitterList.subList(1, resouceSplitterList.size()));
		} else {
			return getOperationId(httpVerb, resourceParent.findResource(CURLY_PATH),
					resouceSplitterList.subList(1, resouceSplitterList.size()));
		}

	}

	public VirtualServiceRequest getResponseType(VirtualServiceRequest mockTransferInput) {
		if (mockTransferInput.getResource() != null) {
			String resource = (mockTransferInput.getResource().substring(0, 1).toUpperCase()
					+ mockTransferInput.getResource().substring(1) + API_SUFFIX).toLowerCase();
			if (mockTransferInput.getOperationId() != null) {
				return mockLoadChoice.get(resource).get(mockTransferInput.getOperationId());
			}
		}
		return null;
	}

	public Map<String, String> getHttpStatusMap() {
		Map<String, String> map = new LinkedHashMap<>();
		for (HttpStatus status : HttpStatus.values()) {
			map.put(String.valueOf(status.value()), status.name());
		}
		return map;
	}

	public void loadMapper() {
		Set<ResourceMapper> resourceMapperList = new LinkedHashSet<>();
		resourceParent = new ResourceMapper(PARENT_ROOT, resourceMapperList);
		ResourceMapper resourceParent = new ResourceMapper(PARENT_ROOT, resourceMapperList);
		for (Entry<String, Map<String, VirtualServiceRequest>> obj : mockLoadChoice.entrySet()) {
			for (Entry<String, VirtualServiceRequest> requestMockObject : obj.getValue().entrySet()) {
				String resource = requestMockObject.getValue().getUrl().substring(1,
						requestMockObject.getValue().getUrl().length());
				List<String> resouceSplitterList = new LinkedList(Arrays.asList(resource.split("/")));
				if (resouceSplitterList.size() > 0) {
					ResourceMapper mapperChild = buildHierarchyObject(requestMockObject.getValue().getMethod(),
							resourceParent, resouceSplitterList, requestMockObject.getKey());
					resourceParent.addResourceMapper(mapperChild);
				}
			}
		}
	}

	public ResourceMapper buildHierarchyObject(String httpVerb, ResourceMapper resourceParent,
			List<String> resouceSplitterList, String operationId) {
		String resource = resouceSplitterList.get(0);
		String actualResource = resouceSplitterList.get(0);
		final Matcher matcher = pattern.matcher(resouceSplitterList.get(0));
		if (matcher.find()) {
			resource = CURLY_PATH;
		}
		if (resouceSplitterList.size() == 1) {
			ResourceMapper resourceMapper = resourceParent.findResource(resource);
			if (resourceMapper == null) {
				resourceMapper = new ResourceMapper(resource);
				resourceMapper.setActualResource(actualResource);
			}
			resourceMapper.setOperationId(httpVerb, operationId);
			return resourceMapper;
		} else if (resourceParent.findResource(resource) != null) {
			ResourceMapper resourceChild = resourceParent.findResource(resource);
			ResourceMapper resourceMapperReturn = buildHierarchyObject(httpVerb, resourceChild,
					resouceSplitterList.subList(1, resouceSplitterList.size()), operationId);
			resourceChild.addResourceMapper(resourceMapperReturn);
			return resourceChild;
		} else {
			Set<ResourceMapper> mapperSet = new LinkedHashSet<>();
			ResourceMapper resourceMapper = new ResourceMapper(resource, mapperSet);
			resourceMapper.setActualResource(actualResource);
			ResourceMapper resourceMapperReturn = buildHierarchyObject(httpVerb, resourceMapper,
					resouceSplitterList.subList(1, resouceSplitterList.size()), operationId);
			resourceMapper.addResourceMapper(resourceMapperReturn);

			return resourceMapper;
		}
	}



}