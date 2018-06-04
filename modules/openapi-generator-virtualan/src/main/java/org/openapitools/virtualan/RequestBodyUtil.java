package org.openapitools.virtualan;

import java.io.IOException;
import java.math.BigDecimal;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.openapitools.virtualan.model.Mock;
import org.openapitools.virtualan.model.MockRequest;
import org.openapitools.virtualan.model.MockResponse;
import org.openapitools.virtualan.model.MockServiceRequest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;

@Service("requestBodyUtil")
public class RequestBodyUtil {

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

	public Object isMockRequestBodyValid(Mock mock) {
		Object object = null;
		Class inputObjectType = virtualServiceInfo.getInputType(mock);
		try {
			if (inputObjectType == null
					&& (mock.getInput() == null || mock.getInput().length() == 0)) {
				object = new String();
				return object ;
			} else if (mock.getInput() != null && mock.getInput().length() > 0
					&& inputObjectType != null) {
				switch (inputObjectType.getTypeName()) {
				case "java.math.BigDecimal":
					object = new BigDecimal(mock.getInput());
					break;
				case "java.lang.Boolean":
					object = new Boolean(mock.getInput());
					break;
				case "java.lang.String":
					object = (String)mock.getInput();
					break;
				case "java.util.Map":
					HashMap<String, String> expectedMap = new ObjectMapper().readValue(mock.getInput(),
							HashMap.class);
					object = expectedMap;
					break;
				case "java.util.List":
					// TO-DO
					break;
				default:
					object = getObjectMapper().readValue(mock.getInput(), inputObjectType);
					break;
				}
/*			} else if (inputObjectType == null
					&& (mockTransferObject.getInput() == null || mockTransferObject.getInput().length() == 0)) {
				isValid = true;
			} else {
				isValid = false;
*/			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		System.out.println(" inputObjectType : " + inputObjectType + " >>> " + object);
		mock.setInputObject(object);
		return object;
	}

	public String getJsonRequestBody(String inputObjectType) {
		try {
			if (inputObjectType != null) {
				switch (Class.forName(inputObjectType).getTypeName()) {
				case "java.math.BigDecimal":
					return "0";
				case "java.lang.Boolean":
					return "true/false";
				case "java.lang.String":
					return "enter valid content..";
				case "java.util.Map":
					return "{\"additionalProp1\": \"string\",  " + "\"additionalProp2\": \"string\",  "
							+ "\"additionalProp3\": \"string\"}";
				case "java.util.List":
					// TO-DO
					break;
				default:
					return objectMapper.writerWithDefaultPrettyPrinter()
							.writeValueAsString(Class.forName(inputObjectType).newInstance());
				}
			}
		} catch (Exception e) {
		}
		return "";
	}

	/*
	 * - @Valid @RequestBody Boolean body - @RequestBody BigDecimal body
	 * - @RequestBody String - @RequestBody Map<String, String> requestBody
	 * - @RequestBody List<String> requestBody
	 */
	public boolean compareRequestBody(Map.Entry<MockRequest, MockResponse> mockRequestResponse,
			MockServiceRequest mockServiceRequest) {
		System.out.println(mockServiceRequest.getInputObject() 
				+" >>>>> "+ mockRequestResponse.getKey().getInput() 
				+" >>>>> "+	mockRequestResponse.getKey().getExcludeSet() 
				+" >>>>> "+ mockServiceRequest.getInputObjectType());
		return compareRequestBody(mockServiceRequest.getInputObject(), mockRequestResponse.getKey().getInput(),
				mockRequestResponse.getKey().getExcludeSet(), mockServiceRequest.getInputObjectType());
	}

	public boolean validateRequestBody(Entry<MockRequest, MockResponse> mockRequestResponse, Class inputObjectType,
			Mock mockTransferObject) {
		return compareRequestBody(mockTransferObject.getInputObject(), mockRequestResponse.getKey().getInput(),
				mockRequestResponse.getKey().getExcludeSet(), inputObjectType);
	}

	private boolean compareRequestBody(Object actualInput, String expectedInput, Collection<String> excludeList,
			Class inputObjectType) {
		try {
			HashMap<String, String> expectedMap =  null;
			switch (inputObjectType.getTypeName()) {
			case "java.math.BigDecimal":
				return new BigDecimal(expectedInput).compareTo((BigDecimal) actualInput) == 0;
			case "java.lang.Boolean":
				return new Boolean(expectedInput).booleanValue() == ((Boolean)actualInput).booleanValue();
			case "java.lang.String":
				return expectedInput.equals(actualInput);
			case "java.util.LinkedHashMap":
				expectedMap = new ObjectMapper().readValue(expectedInput, HashMap.class);
				for (Map.Entry<String, String> actualMap : ((Map<String, String>) actualInput).entrySet()) {
					if (excludeList == null || !excludeList.contains(actualMap.getKey())) {
						if (!actualMap.getValue().equals(expectedMap.get(actualMap.getKey()))) {
							return false;
						}
					}
				}
				return true;
			case "java.util.Map":
				expectedMap = new ObjectMapper().readValue(expectedInput, HashMap.class);
				for (Map.Entry<String, String> actualMap : ((Map<String, String>) actualInput).entrySet()) {
					if (excludeList == null || !excludeList.contains(actualMap.getKey())) {
						if (!actualMap.getValue().equals(expectedMap.get(actualMap.getKey()))) {
							return false;
						}
					}
				}
				return true;				
			case "java.util.HashMap":
				expectedMap = new ObjectMapper().readValue(expectedInput, HashMap.class);
				for (Map.Entry<String, String> actualMap : ((Map<String, String>) actualInput).entrySet()) {
					if (excludeList == null || !excludeList.contains(actualMap.getKey())) {
						if (!actualMap.getValue().equals(expectedMap.get(actualMap.getKey()))) {
							return false;
						}
					}
				}
				return true;				
			case "java.util.List":
				// TO-DO
				break;
			default:
				return EqualsBuilder.reflectionEquals(getObjectMapper().readValue(expectedInput, inputObjectType),
						inputObjectType.cast(actualInput), excludeList);
			}

		} catch (IOException e) { // TO-DO
			// Ignore for now
			System.err.println("Unable to compare body >>>> " + e.getMessage());
		}
		return false;
	}

}
