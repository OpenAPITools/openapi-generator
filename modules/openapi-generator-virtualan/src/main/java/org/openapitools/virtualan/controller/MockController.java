package org.openapitools.virtualan.controller;

import java.io.IOException;
import java.lang.invoke.MethodHandles;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.annotation.PostConstruct;

import org.openapitools.virtualan.VirtualServiceInfo;
import org.openapitools.virtualan.VirtualServiceUtil;
import org.openapitools.virtualan.model.Mock;
import org.openapitools.virtualan.model.MockStatus;
import org.openapitools.virtualan.service.MockService;
import org.springframework.beans.factory.annotation.Autowired;
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
import com.fasterxml.jackson.databind.JsonMappingException;

@RestController
public class MockController {

	@Autowired
	MockService mockService; // Service which will do all data 

	@Autowired
	VirtualServiceInfo virtualServiceInfo;
	
	@Autowired
	private VirtualServiceUtil virtualServiceUtil;

	private final RequestMappingHandlerMapping handlerMapping;

	@Autowired
	public MockController(RequestMappingHandlerMapping handlerMapping) {
		this.handlerMapping = handlerMapping;
	}
	
	@PostConstruct
	public void init() throws ClassNotFoundException, JsonProcessingException, InstantiationException, IllegalAccessException{
		virtualServiceInfo.loadVirtualServices(handlerMapping);
		virtualServiceInfo.loadMapper();
	}
	
	@RequestMapping(value = "/mockload", method = RequestMethod.GET)
	public Map<String, Map<String, Mock>> listAllMockLoadRequest() throws InstantiationException,
			IllegalAccessException, ClassNotFoundException, JsonParseException, JsonMappingException, IOException {
		return virtualServiceInfo.loadVirtualServices(handlerMapping);
	}

	@RequestMapping(value = "/mockservice", method = RequestMethod.GET)
	public ResponseEntity<List<Mock>> listAllMockLoadRequests(){//@RequestParam("resource") String resource, @RequestParam("operationId") String operationId) {
		List<Mock> MockLoadRequests = mockService.findAllMockRequests(); //readByOperationId(resource, operationId);
		if (MockLoadRequests.isEmpty()) {
			return new ResponseEntity<List<Mock>>(HttpStatus.NO_CONTENT);
		}
		return new ResponseEntity<List<Mock>>(MockLoadRequests, HttpStatus.OK);
	}


	@RequestMapping(value = "/mockservice/{id}", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<Mock> getMockLoadRequest(@PathVariable("id") long id) {
		Mock mockLoadRequest = mockService.findById(id);
		if (mockLoadRequest == null) {
			return new ResponseEntity<Mock>(HttpStatus.NOT_FOUND);
		}
		return new ResponseEntity<Mock>(mockLoadRequest, HttpStatus.OK);
	}


	@RequestMapping(value = "/mockservice", method = RequestMethod.POST)
	public ResponseEntity createMockRequest(@RequestBody Mock mockLoadRequest) {// ,UriComponentsBuilder
		try {
			
			if(mockLoadRequest.getHttpStatusCode() == null 
					|| mockLoadRequest.getMethod() == null 
					|| mockLoadRequest.getUrl() == null) {
				return new ResponseEntity<MockStatus>(
						new MockStatus("Check HttpStatusCode or HttpVerb or URL, Please add one of the missing field!!!"),
						HttpStatus.BAD_REQUEST);
			}

			if(mockLoadRequest.getOperationId() == null) {
				String resourceUrl = mockLoadRequest.getUrl().substring(1, mockLoadRequest.getUrl().length());
				List<String> resouceSplitterList = new LinkedList(Arrays.asList(resourceUrl.split("/")));
				if(resouceSplitterList.size() >0) {
					String operationId = virtualServiceInfo.getOperationId( mockLoadRequest.getMethod(), virtualServiceInfo.getResourceParent(), resouceSplitterList);
					mockLoadRequest.setOperationId(operationId);
					mockLoadRequest.setResource(resouceSplitterList.get(0));
					System.out.println( " ORG("+mockLoadRequest.getOperationId()+") >>>>>>>>>>>>>>>> FOUND ("+operationId+") ");
				}
			}
			
			if (!virtualServiceUtil.isMockRequestBodyValid(mockLoadRequest)) {
				return new ResponseEntity<MockStatus>(
						new MockStatus("Check input Json for the Mock Request Body, Please correct the Json!!!"),
						HttpStatus.BAD_REQUEST);
			}

			if (!virtualServiceUtil.isMockResponseBodyValid(mockLoadRequest)) {
				return new ResponseEntity<MockStatus>(
						new MockStatus("Check input Json for the Mock Response Body, Please correct the Json!!!"),
						HttpStatus.BAD_REQUEST);
			}

			if (virtualServiceUtil.isMockAlreadyExists(mockLoadRequest)) {
				return new ResponseEntity<MockStatus>(
						new MockStatus("This Mock request already Present, Please change input Data!!!"),
						HttpStatus.BAD_REQUEST);
			}
			Mock mockTransferObject = mockService.saveMockRequest(mockLoadRequest);
			mockTransferObject.setMockStatus(new MockStatus("Mock created successfully"));
			return new ResponseEntity<>(mockTransferObject, HttpStatus.CREATED);
		} catch (Exception e) {
			e.printStackTrace();
			return new ResponseEntity<MockStatus>(new MockStatus("Unexpected error please retry....." + e.getMessage()),
					HttpStatus.BAD_REQUEST);
		}
		// HttpHeaders headers = new HttpHeaders();
		// headers.setLocation(ucBuilder.path("/mockservice/{id}").buildAndExpand(mockLoadRequest.getId()).toUri());
	}

	@RequestMapping(value = "/mockservice/{id}", method = RequestMethod.PUT)
	public ResponseEntity<Mock> updateMockRequest(@PathVariable("id") long id,
			@RequestBody Mock mockLoadRequest) {

		Mock currentMockLoadRequest = mockService.findById(id);
		if (currentMockLoadRequest == null) {
			return new ResponseEntity<Mock>(HttpStatus.NOT_FOUND);
		}

		currentMockLoadRequest.setInput(mockLoadRequest.getInput());
		currentMockLoadRequest.setOutput(mockLoadRequest.getOutput());
		currentMockLoadRequest.setOperationId(mockLoadRequest.getOperationId());

		mockService.updateMockRequest(currentMockLoadRequest);
		return new ResponseEntity<Mock>(currentMockLoadRequest, HttpStatus.OK);
	}


	@RequestMapping(value = "/mockservice/{id}", method = RequestMethod.DELETE)
	public ResponseEntity<Mock> deleteMockRequest(@PathVariable("id") long id) {
		Mock MockLoadRequest = mockService.findById(id);
		if (MockLoadRequest == null) {
			return new ResponseEntity<Mock>(HttpStatus.NOT_FOUND);
		}
		mockService.deleteMockRequestById(id);
		return new ResponseEntity<Mock>(HttpStatus.NO_CONTENT);
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
		if (fileList.size() == 0) {
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