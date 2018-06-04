package org.openapitools.virtualan;

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

import org.openapitools.virtualan.model.APIResponse;
import org.openapitools.virtualan.model.Mock;
import org.openapitools.virtualan.model.MockKeyValue;
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
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

@Service("virtualServiceInfo")
public class VirtualServiceInfo {

	private static final String CURLY_PATH = "Curly";

	@Autowired
	private ObjectMapper objectMapper;

	@Autowired
	private RequestBodyUtil requestBodyUtil;

	final String rxpCurly = "\\{(.*?)\\}";
	final Pattern pattern = Pattern.compile(rxpCurly, Pattern.MULTILINE);
	ResourceMapper resourceParent;

	public ResourceMapper getResourceParent() {
		return resourceParent;
	}

	public void setResourceParent(ResourceMapper resourceParent) {
		this.resourceParent = resourceParent;
	}

	Map<String, Map<String, Mock>> mockLoadChoice;

	public Map<String, Map<String, Mock>> loadVirtualServices(RequestMappingHandlerMapping handlerMapping)
			throws ClassNotFoundException, JsonProcessingException, InstantiationException, IllegalAccessException {
		if (mockLoadChoice == null) {
			mockLoadChoice = new TreeMap<>();
			Map<RequestMappingInfo, HandlerMethod> mapSwaggerAPI = handlerMapping.getHandlerMethods();
			for (Map.Entry<RequestMappingInfo, HandlerMethod> mapSwaggerAPIEntry : mapSwaggerAPI.entrySet()) {
				if (mapSwaggerAPIEntry.getValue().getBeanType().toString()
						.matches("class org.openapitools.virtualan.api.*Controller")) {// TODO
					String interfaceName = mapSwaggerAPIEntry.getValue().getBeanType().getName().replace("Controller",
							"");
					Class<?> intefaceController = Class.forName(interfaceName);
					interfaceName = interfaceName.toLowerCase();
					interfaceName = interfaceName.substring(interfaceName.lastIndexOf(".") + 1, interfaceName.length());
					if (!mockLoadChoice.containsKey(interfaceName)) {
						String resource = null;
						Api[] apiOperationAnnos = (Api[]) intefaceController.getAnnotationsByType(Api.class);
						if (apiOperationAnnos != null) {
							for (Api api : apiOperationAnnos) {
								if (api.value() != null) {
									resource = api.value();
								}
							}
						}

						Map<String, Mock> mockAPILoadChoice = new LinkedHashMap<String, Mock>();
						for (Method method : intefaceController.getDeclaredMethods()) {
							Annotation[][] annotations = method.getParameterAnnotations();
							Class[] parameterTypes = method.getParameterTypes();
							RequestMapping[] annotInstance = method.getAnnotationsByType(RequestMapping.class);
							Mock mockLoadRequest = new Mock();
							mockLoadRequest.setResource(resource);
							if (annotInstance != null && annotInstance.length > 0) {
								RequestMapping requestMapping = ((RequestMapping) annotInstance[0]);
								if (requestMapping.value() != null && requestMapping.value().length > 0) {
									mockLoadRequest.setUrl(requestMapping.value()[0]);
								}
								if (requestMapping.method() != null && requestMapping.method().length > 0) {
									mockLoadRequest.setMethod(requestMapping.method()[0].name());
								}

								ApiResponses[] apiResponsesAnno = method.getAnnotationsByType(ApiResponses.class);
								if (apiResponsesAnno != null) {
									Map<String, APIResponse> responseType = new HashMap<>();
									for (ApiResponses apiResponses : apiResponsesAnno) {
										for (ApiResponse apiResponse : apiResponses.value()) {
											if (apiResponse.response().getCanonicalName() != null && apiResponse
													.response().getCanonicalName().contains("org.openapitools")) {
												responseType.put(String.valueOf(apiResponse.code()),
														new APIResponse(String.valueOf(apiResponse.code()),
																apiResponse.response().getCanonicalName(),
																objectMapper.writerWithDefaultPrettyPrinter()
																		.writeValueAsString(Class
																				.forName(apiResponse.response()
																						.getCanonicalName())
																				.newInstance()),
																apiResponse.message()));
											} else {
												responseType.put(String.valueOf(apiResponse.code()),
														new APIResponse(String.valueOf(apiResponse.code()), null, null,
																apiResponse.message()));
											}
											mockLoadRequest.setResponseType(responseType);
										}
									}

								}

								int i = 0;
								List<MockKeyValue> availableParams = new ArrayList<MockKeyValue>();

								for (Annotation[] anns : annotations) {
									Class<?> parameterType = parameterTypes[i++];
									for (Annotation paramAnnotation : anns) {
										if (paramAnnotation.annotationType().equals(RequestParam.class)) {
											RequestParam requestParam = (RequestParam) paramAnnotation;
											availableParams.add(new MockKeyValue(requestParam.value(), null));
										} else if (paramAnnotation.annotationType().equals(PathVariable.class)) {
											PathVariable pathVariable = (PathVariable) paramAnnotation;
											availableParams.add(new MockKeyValue(pathVariable.value(), null));
										} else if (paramAnnotation.annotationType().equals(RequestBody.class)) {
											mockLoadRequest.setInputObjectType(parameterType);
											mockLoadRequest.setInput(
													requestBodyUtil.getJsonRequestBody(parameterType.getName()));
										}
									}
								}
								// TO build Return Json object
								//String returnObject = method.getGenericReturnType().getTypeName();
								mockLoadRequest.setOperationId(method.getName());
								mockLoadRequest.setAvailableParams(availableParams);
								mockLoadRequest.setHttpStatusMap(getHttpStatusMap());
								mockAPILoadChoice.put(method.getName(), mockLoadRequest);
							}
						}
						mockLoadChoice.put(interfaceName, mockAPILoadChoice);
					}
				}
			}
		}
		return mockLoadChoice;

	}

	public Class<?> getInputType(Mock mockTransferInput) {
		Class<?> inputType = null;
		if (mockTransferInput.getResource() != null) {
			String resource = (mockTransferInput.getResource().substring(0, 1).toUpperCase()
					+ mockTransferInput.getResource().substring(1) + "Api").replaceAll("[^a-zA-Z0-9]", "")
							.toLowerCase();
			if (mockTransferInput.getOperationId() != null) {
				Mock mockTransferActual = mockLoadChoice.get(resource).get(mockTransferInput.getOperationId());
				if (mockTransferActual != null) {
					inputType = mockTransferActual.getInputObjectType();
				}
			} else {
				String resourceUrl = mockTransferInput.getUrl().substring(1, mockTransferInput.getUrl().length());
				List<String> resouceSplitterList = new LinkedList<String>(Arrays.asList(resourceUrl.split("/")));
				if (resouceSplitterList.size() > 0) {
					String operationId = getOperationId(mockTransferInput.getMethod(), resourceParent,
							resouceSplitterList);

					Mock mockTransferActual = mockLoadChoice.get(resource).get(operationId);
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

	public Mock getResponseType(Mock mockTransferInput) {
		if (mockTransferInput.getResource() != null) {
			String resource = (mockTransferInput.getResource().substring(0, 1).toUpperCase()
					+ mockTransferInput.getResource().substring(1) + "Api").replaceAll("[^a-zA-Z0-9]", "")
							.toLowerCase();
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
		resourceParent = new ResourceMapper("Parent-Root", resourceMapperList);
		ResourceMapper resourceParent = new ResourceMapper("Parent-Root", resourceMapperList);
		for (Entry<String, Map<String, Mock>> obj : mockLoadChoice.entrySet()) {
			for (Entry<String, Mock> requestMockObject : obj.getValue().entrySet()) {
				String resource = requestMockObject.getValue().getUrl().substring(1,
						requestMockObject.getValue().getUrl().length());

				List<String> resouceSplitterList = new LinkedList<String>(Arrays.asList(resource.split("/")));
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

	class ResourceMapper {
		String resource;
		String actualResource;
		Set<ResourceMapper> resourceMapperList = new LinkedHashSet<>();
		Map<String, String> operationIdMap = new HashMap<>();

		ResourceMapper(String resource, Set<ResourceMapper> resourceMapperList) {
			this.resource = resource;
			this.resourceMapperList = resourceMapperList;
		}

		ResourceMapper(String resource) {
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
			result = prime * result + getOuterType().hashCode();
			result = prime * result + ((resource == null) ? 0 : resource.hashCode());
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
			if (!getOuterType().equals(other.getOuterType()))
				return false;
			if (resource == null) {
				if (other.resource != null)
					return false;
			} else if (!resource.equals(other.resource))
				return false;
			return true;
		}

		private VirtualServiceInfo getOuterType() {
			return VirtualServiceInfo.this;
		}

	}

}
