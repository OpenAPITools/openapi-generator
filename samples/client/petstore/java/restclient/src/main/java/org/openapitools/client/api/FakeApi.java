package org.openapitools.client.api;

import org.openapitools.client.ApiClient;

import java.math.BigDecimal;
import org.openapitools.client.model.ChildWithNullable;
import org.openapitools.client.model.Client;
import org.openapitools.client.model.EnumClass;
import org.openapitools.client.model.FakeBigDecimalMap200Response;
import java.io.File;
import org.openapitools.client.model.FileSchemaTestClass;
import org.openapitools.client.model.HealthCheckResult;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import org.openapitools.client.model.OuterComposite;
import org.openapitools.client.model.OuterObjectWithEnumProperty;
import org.openapitools.client.model.Pet;
import org.openapitools.client.model.TestInlineFreeformAdditionalPropertiesRequest;
import org.openapitools.client.model.User;

import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.web.client.RestClient.ResponseSpec;
import org.springframework.web.client.RestClientResponseException;
import org.springframework.core.io.FileSystemResource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen", comments = "Generator version: 7.7.0-SNAPSHOT")
public class FakeApi {
    private ApiClient apiClient;

    public FakeApi() {
        this(new ApiClient());
    }

    @Autowired
    public FakeApi(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    public ApiClient getApiClient() {
        return apiClient;
    }

    public void setApiClient(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    /**
     * 
     * for Java apache and Java native, test toUrlQueryString for maps with BegDecimal keys
     * <p><b>200</b> - successful operation
     * @return FakeBigDecimalMap200Response
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec fakeBigDecimalMapRequestCreation() throws RestClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { 
            "*/*"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<FakeBigDecimalMap200Response> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/fake/BigDecimalMap", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * for Java apache and Java native, test toUrlQueryString for maps with BegDecimal keys
     * <p><b>200</b> - successful operation
     * @return FakeBigDecimalMap200Response
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public FakeBigDecimalMap200Response fakeBigDecimalMap() throws RestClientResponseException {
        ParameterizedTypeReference<FakeBigDecimalMap200Response> localVarReturnType = new ParameterizedTypeReference<>() {};
        return fakeBigDecimalMapRequestCreation().body(localVarReturnType);
    }

    /**
     * 
     * for Java apache and Java native, test toUrlQueryString for maps with BegDecimal keys
     * <p><b>200</b> - successful operation
     * @return ResponseEntity&lt;FakeBigDecimalMap200Response&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<FakeBigDecimalMap200Response> fakeBigDecimalMapWithHttpInfo() throws RestClientResponseException {
        ParameterizedTypeReference<FakeBigDecimalMap200Response> localVarReturnType = new ParameterizedTypeReference<>() {};
        return fakeBigDecimalMapRequestCreation().toEntity(localVarReturnType);
    }

    /**
     * 
     * for Java apache and Java native, test toUrlQueryString for maps with BegDecimal keys
     * <p><b>200</b> - successful operation
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec fakeBigDecimalMapWithResponseSpec() throws RestClientResponseException {
        return fakeBigDecimalMapRequestCreation();
    }
    /**
     * Health check endpoint
     * 
     * <p><b>200</b> - The instance started successfully
     * @return HealthCheckResult
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec fakeHealthGetRequestCreation() throws RestClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { 
            "application/json"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<HealthCheckResult> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/fake/health", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Health check endpoint
     * 
     * <p><b>200</b> - The instance started successfully
     * @return HealthCheckResult
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public HealthCheckResult fakeHealthGet() throws RestClientResponseException {
        ParameterizedTypeReference<HealthCheckResult> localVarReturnType = new ParameterizedTypeReference<>() {};
        return fakeHealthGetRequestCreation().body(localVarReturnType);
    }

    /**
     * Health check endpoint
     * 
     * <p><b>200</b> - The instance started successfully
     * @return ResponseEntity&lt;HealthCheckResult&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<HealthCheckResult> fakeHealthGetWithHttpInfo() throws RestClientResponseException {
        ParameterizedTypeReference<HealthCheckResult> localVarReturnType = new ParameterizedTypeReference<>() {};
        return fakeHealthGetRequestCreation().toEntity(localVarReturnType);
    }

    /**
     * Health check endpoint
     * 
     * <p><b>200</b> - The instance started successfully
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec fakeHealthGetWithResponseSpec() throws RestClientResponseException {
        return fakeHealthGetRequestCreation();
    }
    /**
     * test http signature authentication
     * 
     * <p><b>200</b> - The instance started successfully
     * @param pet Pet object that needs to be added to the store
     * @param query1 query parameter
     * @param header1 header parameter
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec fakeHttpSignatureTestRequestCreation(Pet pet, String query1, String header1) throws RestClientResponseException {
        Object postBody = pet;
        // verify the required parameter 'pet' is set
        if (pet == null) {
            throw new RestClientResponseException("Missing the required parameter 'pet' when calling fakeHttpSignatureTest", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "query_1", query1));
        

        if (header1 != null)
        headerParams.add("header_1", apiClient.parameterToString(header1));
        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json", "application/xml"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] { "http_signature_test" };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/fake/http-signature-test", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * test http signature authentication
     * 
     * <p><b>200</b> - The instance started successfully
     * @param pet Pet object that needs to be added to the store
     * @param query1 query parameter
     * @param header1 header parameter
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public void fakeHttpSignatureTest(Pet pet, String query1, String header1) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        fakeHttpSignatureTestRequestCreation(pet, query1, header1).body(localVarReturnType);
    }

    /**
     * test http signature authentication
     * 
     * <p><b>200</b> - The instance started successfully
     * @param pet Pet object that needs to be added to the store
     * @param query1 query parameter
     * @param header1 header parameter
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> fakeHttpSignatureTestWithHttpInfo(Pet pet, String query1, String header1) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return fakeHttpSignatureTestRequestCreation(pet, query1, header1).toEntity(localVarReturnType);
    }

    /**
     * test http signature authentication
     * 
     * <p><b>200</b> - The instance started successfully
     * @param pet Pet object that needs to be added to the store
     * @param query1 query parameter
     * @param header1 header parameter
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec fakeHttpSignatureTestWithResponseSpec(Pet pet, String query1, String header1) throws RestClientResponseException {
        return fakeHttpSignatureTestRequestCreation(pet, query1, header1);
    }
    /**
     * 
     * Test serialization of outer boolean types
     * <p><b>200</b> - Output boolean
     * @param body Input boolean as post body
     * @return Boolean
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec fakeOuterBooleanSerializeRequestCreation(Boolean body) throws RestClientResponseException {
        Object postBody = body;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { 
            "*/*"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Boolean> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/fake/outer/boolean", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * Test serialization of outer boolean types
     * <p><b>200</b> - Output boolean
     * @param body Input boolean as post body
     * @return Boolean
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public Boolean fakeOuterBooleanSerialize(Boolean body) throws RestClientResponseException {
        ParameterizedTypeReference<Boolean> localVarReturnType = new ParameterizedTypeReference<>() {};
        return fakeOuterBooleanSerializeRequestCreation(body).body(localVarReturnType);
    }

    /**
     * 
     * Test serialization of outer boolean types
     * <p><b>200</b> - Output boolean
     * @param body Input boolean as post body
     * @return ResponseEntity&lt;Boolean&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Boolean> fakeOuterBooleanSerializeWithHttpInfo(Boolean body) throws RestClientResponseException {
        ParameterizedTypeReference<Boolean> localVarReturnType = new ParameterizedTypeReference<>() {};
        return fakeOuterBooleanSerializeRequestCreation(body).toEntity(localVarReturnType);
    }

    /**
     * 
     * Test serialization of outer boolean types
     * <p><b>200</b> - Output boolean
     * @param body Input boolean as post body
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec fakeOuterBooleanSerializeWithResponseSpec(Boolean body) throws RestClientResponseException {
        return fakeOuterBooleanSerializeRequestCreation(body);
    }
    /**
     * 
     * Test serialization of object with outer number type
     * <p><b>200</b> - Output composite
     * @param outerComposite Input composite as post body
     * @return OuterComposite
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec fakeOuterCompositeSerializeRequestCreation(OuterComposite outerComposite) throws RestClientResponseException {
        Object postBody = outerComposite;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { 
            "*/*"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<OuterComposite> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/fake/outer/composite", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * Test serialization of object with outer number type
     * <p><b>200</b> - Output composite
     * @param outerComposite Input composite as post body
     * @return OuterComposite
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public OuterComposite fakeOuterCompositeSerialize(OuterComposite outerComposite) throws RestClientResponseException {
        ParameterizedTypeReference<OuterComposite> localVarReturnType = new ParameterizedTypeReference<>() {};
        return fakeOuterCompositeSerializeRequestCreation(outerComposite).body(localVarReturnType);
    }

    /**
     * 
     * Test serialization of object with outer number type
     * <p><b>200</b> - Output composite
     * @param outerComposite Input composite as post body
     * @return ResponseEntity&lt;OuterComposite&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<OuterComposite> fakeOuterCompositeSerializeWithHttpInfo(OuterComposite outerComposite) throws RestClientResponseException {
        ParameterizedTypeReference<OuterComposite> localVarReturnType = new ParameterizedTypeReference<>() {};
        return fakeOuterCompositeSerializeRequestCreation(outerComposite).toEntity(localVarReturnType);
    }

    /**
     * 
     * Test serialization of object with outer number type
     * <p><b>200</b> - Output composite
     * @param outerComposite Input composite as post body
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec fakeOuterCompositeSerializeWithResponseSpec(OuterComposite outerComposite) throws RestClientResponseException {
        return fakeOuterCompositeSerializeRequestCreation(outerComposite);
    }
    /**
     * 
     * Test serialization of outer number types
     * <p><b>200</b> - Output number
     * @param body Input number as post body
     * @return BigDecimal
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec fakeOuterNumberSerializeRequestCreation(BigDecimal body) throws RestClientResponseException {
        Object postBody = body;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { 
            "*/*"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<BigDecimal> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/fake/outer/number", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * Test serialization of outer number types
     * <p><b>200</b> - Output number
     * @param body Input number as post body
     * @return BigDecimal
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public BigDecimal fakeOuterNumberSerialize(BigDecimal body) throws RestClientResponseException {
        ParameterizedTypeReference<BigDecimal> localVarReturnType = new ParameterizedTypeReference<>() {};
        return fakeOuterNumberSerializeRequestCreation(body).body(localVarReturnType);
    }

    /**
     * 
     * Test serialization of outer number types
     * <p><b>200</b> - Output number
     * @param body Input number as post body
     * @return ResponseEntity&lt;BigDecimal&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<BigDecimal> fakeOuterNumberSerializeWithHttpInfo(BigDecimal body) throws RestClientResponseException {
        ParameterizedTypeReference<BigDecimal> localVarReturnType = new ParameterizedTypeReference<>() {};
        return fakeOuterNumberSerializeRequestCreation(body).toEntity(localVarReturnType);
    }

    /**
     * 
     * Test serialization of outer number types
     * <p><b>200</b> - Output number
     * @param body Input number as post body
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec fakeOuterNumberSerializeWithResponseSpec(BigDecimal body) throws RestClientResponseException {
        return fakeOuterNumberSerializeRequestCreation(body);
    }
    /**
     * 
     * Test serialization of outer string types
     * <p><b>200</b> - Output string
     * @param body Input string as post body
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec fakeOuterStringSerializeRequestCreation(String body) throws RestClientResponseException {
        Object postBody = body;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { 
            "*/*"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/fake/outer/string", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * Test serialization of outer string types
     * <p><b>200</b> - Output string
     * @param body Input string as post body
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public String fakeOuterStringSerialize(String body) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return fakeOuterStringSerializeRequestCreation(body).body(localVarReturnType);
    }

    /**
     * 
     * Test serialization of outer string types
     * <p><b>200</b> - Output string
     * @param body Input string as post body
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> fakeOuterStringSerializeWithHttpInfo(String body) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return fakeOuterStringSerializeRequestCreation(body).toEntity(localVarReturnType);
    }

    /**
     * 
     * Test serialization of outer string types
     * <p><b>200</b> - Output string
     * @param body Input string as post body
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec fakeOuterStringSerializeWithResponseSpec(String body) throws RestClientResponseException {
        return fakeOuterStringSerializeRequestCreation(body);
    }
    /**
     * 
     * Test serialization of enum (int) properties with examples
     * <p><b>200</b> - Output enum (int)
     * @param outerObjectWithEnumProperty Input enum (int) as post body
     * @return OuterObjectWithEnumProperty
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec fakePropertyEnumIntegerSerializeRequestCreation(OuterObjectWithEnumProperty outerObjectWithEnumProperty) throws RestClientResponseException {
        Object postBody = outerObjectWithEnumProperty;
        // verify the required parameter 'outerObjectWithEnumProperty' is set
        if (outerObjectWithEnumProperty == null) {
            throw new RestClientResponseException("Missing the required parameter 'outerObjectWithEnumProperty' when calling fakePropertyEnumIntegerSerialize", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { 
            "*/*"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<OuterObjectWithEnumProperty> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/fake/property/enum-int", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * Test serialization of enum (int) properties with examples
     * <p><b>200</b> - Output enum (int)
     * @param outerObjectWithEnumProperty Input enum (int) as post body
     * @return OuterObjectWithEnumProperty
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public OuterObjectWithEnumProperty fakePropertyEnumIntegerSerialize(OuterObjectWithEnumProperty outerObjectWithEnumProperty) throws RestClientResponseException {
        ParameterizedTypeReference<OuterObjectWithEnumProperty> localVarReturnType = new ParameterizedTypeReference<>() {};
        return fakePropertyEnumIntegerSerializeRequestCreation(outerObjectWithEnumProperty).body(localVarReturnType);
    }

    /**
     * 
     * Test serialization of enum (int) properties with examples
     * <p><b>200</b> - Output enum (int)
     * @param outerObjectWithEnumProperty Input enum (int) as post body
     * @return ResponseEntity&lt;OuterObjectWithEnumProperty&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<OuterObjectWithEnumProperty> fakePropertyEnumIntegerSerializeWithHttpInfo(OuterObjectWithEnumProperty outerObjectWithEnumProperty) throws RestClientResponseException {
        ParameterizedTypeReference<OuterObjectWithEnumProperty> localVarReturnType = new ParameterizedTypeReference<>() {};
        return fakePropertyEnumIntegerSerializeRequestCreation(outerObjectWithEnumProperty).toEntity(localVarReturnType);
    }

    /**
     * 
     * Test serialization of enum (int) properties with examples
     * <p><b>200</b> - Output enum (int)
     * @param outerObjectWithEnumProperty Input enum (int) as post body
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec fakePropertyEnumIntegerSerializeWithResponseSpec(OuterObjectWithEnumProperty outerObjectWithEnumProperty) throws RestClientResponseException {
        return fakePropertyEnumIntegerSerializeRequestCreation(outerObjectWithEnumProperty);
    }
    /**
     * test referenced additionalProperties
     * 
     * <p><b>200</b> - successful operation
     * @param requestBody request body
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testAdditionalPropertiesReferenceRequestCreation(Map<String, Object> requestBody) throws RestClientResponseException {
        Object postBody = requestBody;
        // verify the required parameter 'requestBody' is set
        if (requestBody == null) {
            throw new RestClientResponseException("Missing the required parameter 'requestBody' when calling testAdditionalPropertiesReference", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/fake/additionalProperties-reference", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * test referenced additionalProperties
     * 
     * <p><b>200</b> - successful operation
     * @param requestBody request body
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public void testAdditionalPropertiesReference(Map<String, Object> requestBody) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        testAdditionalPropertiesReferenceRequestCreation(requestBody).body(localVarReturnType);
    }

    /**
     * test referenced additionalProperties
     * 
     * <p><b>200</b> - successful operation
     * @param requestBody request body
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> testAdditionalPropertiesReferenceWithHttpInfo(Map<String, Object> requestBody) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testAdditionalPropertiesReferenceRequestCreation(requestBody).toEntity(localVarReturnType);
    }

    /**
     * test referenced additionalProperties
     * 
     * <p><b>200</b> - successful operation
     * @param requestBody request body
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testAdditionalPropertiesReferenceWithResponseSpec(Map<String, Object> requestBody) throws RestClientResponseException {
        return testAdditionalPropertiesReferenceRequestCreation(requestBody);
    }
    /**
     * 
     * For this test, the body has to be a binary file.
     * <p><b>200</b> - Success
     * @param body image to upload
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testBodyWithBinaryRequestCreation(File body) throws RestClientResponseException {
        Object postBody = body;
        // verify the required parameter 'body' is set
        if (body == null) {
            throw new RestClientResponseException("Missing the required parameter 'body' when calling testBodyWithBinary", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "image/png"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/fake/body-with-binary", HttpMethod.PUT, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * For this test, the body has to be a binary file.
     * <p><b>200</b> - Success
     * @param body image to upload
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public void testBodyWithBinary(File body) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        testBodyWithBinaryRequestCreation(body).body(localVarReturnType);
    }

    /**
     * 
     * For this test, the body has to be a binary file.
     * <p><b>200</b> - Success
     * @param body image to upload
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> testBodyWithBinaryWithHttpInfo(File body) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testBodyWithBinaryRequestCreation(body).toEntity(localVarReturnType);
    }

    /**
     * 
     * For this test, the body has to be a binary file.
     * <p><b>200</b> - Success
     * @param body image to upload
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testBodyWithBinaryWithResponseSpec(File body) throws RestClientResponseException {
        return testBodyWithBinaryRequestCreation(body);
    }
    /**
     * 
     * For this test, the body for this request must reference a schema named &#x60;File&#x60;.
     * <p><b>200</b> - Success
     * @param fileSchemaTestClass The fileSchemaTestClass parameter
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testBodyWithFileSchemaRequestCreation(FileSchemaTestClass fileSchemaTestClass) throws RestClientResponseException {
        Object postBody = fileSchemaTestClass;
        // verify the required parameter 'fileSchemaTestClass' is set
        if (fileSchemaTestClass == null) {
            throw new RestClientResponseException("Missing the required parameter 'fileSchemaTestClass' when calling testBodyWithFileSchema", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/fake/body-with-file-schema", HttpMethod.PUT, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * For this test, the body for this request must reference a schema named &#x60;File&#x60;.
     * <p><b>200</b> - Success
     * @param fileSchemaTestClass The fileSchemaTestClass parameter
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public void testBodyWithFileSchema(FileSchemaTestClass fileSchemaTestClass) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        testBodyWithFileSchemaRequestCreation(fileSchemaTestClass).body(localVarReturnType);
    }

    /**
     * 
     * For this test, the body for this request must reference a schema named &#x60;File&#x60;.
     * <p><b>200</b> - Success
     * @param fileSchemaTestClass The fileSchemaTestClass parameter
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> testBodyWithFileSchemaWithHttpInfo(FileSchemaTestClass fileSchemaTestClass) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testBodyWithFileSchemaRequestCreation(fileSchemaTestClass).toEntity(localVarReturnType);
    }

    /**
     * 
     * For this test, the body for this request must reference a schema named &#x60;File&#x60;.
     * <p><b>200</b> - Success
     * @param fileSchemaTestClass The fileSchemaTestClass parameter
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testBodyWithFileSchemaWithResponseSpec(FileSchemaTestClass fileSchemaTestClass) throws RestClientResponseException {
        return testBodyWithFileSchemaRequestCreation(fileSchemaTestClass);
    }
    /**
     * 
     * 
     * <p><b>200</b> - Success
     * @param query The query parameter
     * @param user The user parameter
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testBodyWithQueryParamsRequestCreation(String query, User user) throws RestClientResponseException {
        Object postBody = user;
        // verify the required parameter 'query' is set
        if (query == null) {
            throw new RestClientResponseException("Missing the required parameter 'query' when calling testBodyWithQueryParams", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // verify the required parameter 'user' is set
        if (user == null) {
            throw new RestClientResponseException("Missing the required parameter 'user' when calling testBodyWithQueryParams", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "query", query));
        
        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/fake/body-with-query-params", HttpMethod.PUT, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - Success
     * @param query The query parameter
     * @param user The user parameter
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public void testBodyWithQueryParams(String query, User user) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        testBodyWithQueryParamsRequestCreation(query, user).body(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - Success
     * @param query The query parameter
     * @param user The user parameter
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> testBodyWithQueryParamsWithHttpInfo(String query, User user) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testBodyWithQueryParamsRequestCreation(query, user).toEntity(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - Success
     * @param query The query parameter
     * @param user The user parameter
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testBodyWithQueryParamsWithResponseSpec(String query, User user) throws RestClientResponseException {
        return testBodyWithQueryParamsRequestCreation(query, user);
    }
    /**
     * To test \&quot;client\&quot; model
     * To test \&quot;client\&quot; model
     * <p><b>200</b> - successful operation
     * @param client client model
     * @return Client
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testClientModelRequestCreation(Client client) throws RestClientResponseException {
        Object postBody = client;
        // verify the required parameter 'client' is set
        if (client == null) {
            throw new RestClientResponseException("Missing the required parameter 'client' when calling testClientModel", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { 
            "application/json"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Client> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/fake", HttpMethod.PATCH, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * To test \&quot;client\&quot; model
     * To test \&quot;client\&quot; model
     * <p><b>200</b> - successful operation
     * @param client client model
     * @return Client
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public Client testClientModel(Client client) throws RestClientResponseException {
        ParameterizedTypeReference<Client> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testClientModelRequestCreation(client).body(localVarReturnType);
    }

    /**
     * To test \&quot;client\&quot; model
     * To test \&quot;client\&quot; model
     * <p><b>200</b> - successful operation
     * @param client client model
     * @return ResponseEntity&lt;Client&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Client> testClientModelWithHttpInfo(Client client) throws RestClientResponseException {
        ParameterizedTypeReference<Client> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testClientModelRequestCreation(client).toEntity(localVarReturnType);
    }

    /**
     * To test \&quot;client\&quot; model
     * To test \&quot;client\&quot; model
     * <p><b>200</b> - successful operation
     * @param client client model
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testClientModelWithResponseSpec(Client client) throws RestClientResponseException {
        return testClientModelRequestCreation(client);
    }
    /**
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     * <p><b>400</b> - Invalid username supplied
     * <p><b>404</b> - User not found
     * @param number None
     * @param _double None
     * @param patternWithoutDelimiter None
     * @param _byte None
     * @param integer None
     * @param int32 None
     * @param int64 None
     * @param _float None
     * @param string None
     * @param binary None
     * @param date None
     * @param dateTime None
     * @param password None
     * @param paramCallback None
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testEndpointParametersRequestCreation(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string, File binary, LocalDate date, OffsetDateTime dateTime, String password, String paramCallback) throws RestClientResponseException {
        Object postBody = null;
        // verify the required parameter 'number' is set
        if (number == null) {
            throw new RestClientResponseException("Missing the required parameter 'number' when calling testEndpointParameters", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // verify the required parameter '_double' is set
        if (_double == null) {
            throw new RestClientResponseException("Missing the required parameter '_double' when calling testEndpointParameters", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // verify the required parameter 'patternWithoutDelimiter' is set
        if (patternWithoutDelimiter == null) {
            throw new RestClientResponseException("Missing the required parameter 'patternWithoutDelimiter' when calling testEndpointParameters", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // verify the required parameter '_byte' is set
        if (_byte == null) {
            throw new RestClientResponseException("Missing the required parameter '_byte' when calling testEndpointParameters", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        if (integer != null)
            formParams.add("integer", integer);
        if (int32 != null)
            formParams.add("int32", int32);
        if (int64 != null)
            formParams.add("int64", int64);
        if (number != null)
            formParams.add("number", number);
        if (_float != null)
            formParams.add("float", _float);
        if (_double != null)
            formParams.add("double", _double);
        if (string != null)
            formParams.add("string", string);
        if (patternWithoutDelimiter != null)
            formParams.add("pattern_without_delimiter", patternWithoutDelimiter);
        if (_byte != null)
            formParams.add("byte", _byte);
        if (binary != null)
            formParams.add("binary", new FileSystemResource(binary));
        if (date != null)
            formParams.add("date", date);
        if (dateTime != null)
            formParams.add("dateTime", dateTime);
        if (password != null)
            formParams.add("password", password);
        if (paramCallback != null)
            formParams.add("callback", paramCallback);

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/x-www-form-urlencoded"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] { "http_basic_test" };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/fake", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     * <p><b>400</b> - Invalid username supplied
     * <p><b>404</b> - User not found
     * @param number None
     * @param _double None
     * @param patternWithoutDelimiter None
     * @param _byte None
     * @param integer None
     * @param int32 None
     * @param int64 None
     * @param _float None
     * @param string None
     * @param binary None
     * @param date None
     * @param dateTime None
     * @param password None
     * @param paramCallback None
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public void testEndpointParameters(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string, File binary, LocalDate date, OffsetDateTime dateTime, String password, String paramCallback) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        testEndpointParametersRequestCreation(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, paramCallback).body(localVarReturnType);
    }

    /**
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     * <p><b>400</b> - Invalid username supplied
     * <p><b>404</b> - User not found
     * @param number None
     * @param _double None
     * @param patternWithoutDelimiter None
     * @param _byte None
     * @param integer None
     * @param int32 None
     * @param int64 None
     * @param _float None
     * @param string None
     * @param binary None
     * @param date None
     * @param dateTime None
     * @param password None
     * @param paramCallback None
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> testEndpointParametersWithHttpInfo(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string, File binary, LocalDate date, OffsetDateTime dateTime, String password, String paramCallback) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testEndpointParametersRequestCreation(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, paramCallback).toEntity(localVarReturnType);
    }

    /**
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     * <p><b>400</b> - Invalid username supplied
     * <p><b>404</b> - User not found
     * @param number None
     * @param _double None
     * @param patternWithoutDelimiter None
     * @param _byte None
     * @param integer None
     * @param int32 None
     * @param int64 None
     * @param _float None
     * @param string None
     * @param binary None
     * @param date None
     * @param dateTime None
     * @param password None
     * @param paramCallback None
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testEndpointParametersWithResponseSpec(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string, File binary, LocalDate date, OffsetDateTime dateTime, String password, String paramCallback) throws RestClientResponseException {
        return testEndpointParametersRequestCreation(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, paramCallback);
    }
    /**
     * To test enum parameters
     * To test enum parameters
     * <p><b>400</b> - Invalid request
     * <p><b>404</b> - Not found
     * @param enumHeaderStringArray Header parameter enum test (string array)
     * @param enumHeaderString Header parameter enum test (string)
     * @param enumQueryStringArray Query parameter enum test (string array)
     * @param enumQueryString Query parameter enum test (string)
     * @param enumQueryInteger Query parameter enum test (double)
     * @param enumQueryDouble Query parameter enum test (double)
     * @param enumQueryModelArray The enumQueryModelArray parameter
     * @param enumFormStringArray Form parameter enum test (string array)
     * @param enumFormString Form parameter enum test (string)
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testEnumParametersRequestCreation(List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble, List<EnumClass> enumQueryModelArray, List<String> enumFormStringArray, String enumFormString) throws RestClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        queryParams.putAll(apiClient.parameterToMultiValueMap(ApiClient.CollectionFormat.valueOf("multi".toUpperCase(Locale.ROOT)), "enum_query_string_array", enumQueryStringArray));
        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "enum_query_string", enumQueryString));
        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "enum_query_integer", enumQueryInteger));
        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "enum_query_double", enumQueryDouble));
        queryParams.putAll(apiClient.parameterToMultiValueMap(ApiClient.CollectionFormat.valueOf("multi".toUpperCase(Locale.ROOT)), "enum_query_model_array", enumQueryModelArray));
        

        if (enumHeaderStringArray != null)
        headerParams.add("enum_header_string_array", apiClient.parameterToString(enumHeaderStringArray));
        if (enumHeaderString != null)
        headerParams.add("enum_header_string", apiClient.parameterToString(enumHeaderString));
        if (enumFormStringArray != null)
            formParams.addAll("enum_form_string_array", enumFormStringArray);
        if (enumFormString != null)
            formParams.add("enum_form_string", enumFormString);

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/x-www-form-urlencoded"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/fake", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * To test enum parameters
     * To test enum parameters
     * <p><b>400</b> - Invalid request
     * <p><b>404</b> - Not found
     * @param enumHeaderStringArray Header parameter enum test (string array)
     * @param enumHeaderString Header parameter enum test (string)
     * @param enumQueryStringArray Query parameter enum test (string array)
     * @param enumQueryString Query parameter enum test (string)
     * @param enumQueryInteger Query parameter enum test (double)
     * @param enumQueryDouble Query parameter enum test (double)
     * @param enumQueryModelArray The enumQueryModelArray parameter
     * @param enumFormStringArray Form parameter enum test (string array)
     * @param enumFormString Form parameter enum test (string)
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public void testEnumParameters(List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble, List<EnumClass> enumQueryModelArray, List<String> enumFormStringArray, String enumFormString) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        testEnumParametersRequestCreation(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumQueryModelArray, enumFormStringArray, enumFormString).body(localVarReturnType);
    }

    /**
     * To test enum parameters
     * To test enum parameters
     * <p><b>400</b> - Invalid request
     * <p><b>404</b> - Not found
     * @param enumHeaderStringArray Header parameter enum test (string array)
     * @param enumHeaderString Header parameter enum test (string)
     * @param enumQueryStringArray Query parameter enum test (string array)
     * @param enumQueryString Query parameter enum test (string)
     * @param enumQueryInteger Query parameter enum test (double)
     * @param enumQueryDouble Query parameter enum test (double)
     * @param enumQueryModelArray The enumQueryModelArray parameter
     * @param enumFormStringArray Form parameter enum test (string array)
     * @param enumFormString Form parameter enum test (string)
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> testEnumParametersWithHttpInfo(List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble, List<EnumClass> enumQueryModelArray, List<String> enumFormStringArray, String enumFormString) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testEnumParametersRequestCreation(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumQueryModelArray, enumFormStringArray, enumFormString).toEntity(localVarReturnType);
    }

    /**
     * To test enum parameters
     * To test enum parameters
     * <p><b>400</b> - Invalid request
     * <p><b>404</b> - Not found
     * @param enumHeaderStringArray Header parameter enum test (string array)
     * @param enumHeaderString Header parameter enum test (string)
     * @param enumQueryStringArray Query parameter enum test (string array)
     * @param enumQueryString Query parameter enum test (string)
     * @param enumQueryInteger Query parameter enum test (double)
     * @param enumQueryDouble Query parameter enum test (double)
     * @param enumQueryModelArray The enumQueryModelArray parameter
     * @param enumFormStringArray Form parameter enum test (string array)
     * @param enumFormString Form parameter enum test (string)
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testEnumParametersWithResponseSpec(List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble, List<EnumClass> enumQueryModelArray, List<String> enumFormStringArray, String enumFormString) throws RestClientResponseException {
        return testEnumParametersRequestCreation(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumQueryModelArray, enumFormStringArray, enumFormString);
    }
    /**
     * Fake endpoint to test group parameters (optional)
     * Fake endpoint to test group parameters (optional)
     * <p><b>400</b> - Something wrong
     * @param requiredStringGroup Required String in group parameters
     * @param requiredBooleanGroup Required Boolean in group parameters
     * @param requiredInt64Group Required Integer in group parameters
     * @param stringGroup String in group parameters
     * @param booleanGroup Boolean in group parameters
     * @param int64Group Integer in group parameters
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testGroupParametersRequestCreation(Integer requiredStringGroup, Boolean requiredBooleanGroup, Long requiredInt64Group, Integer stringGroup, Boolean booleanGroup, Long int64Group) throws RestClientResponseException {
        Object postBody = null;
        // verify the required parameter 'requiredStringGroup' is set
        if (requiredStringGroup == null) {
            throw new RestClientResponseException("Missing the required parameter 'requiredStringGroup' when calling testGroupParameters", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // verify the required parameter 'requiredBooleanGroup' is set
        if (requiredBooleanGroup == null) {
            throw new RestClientResponseException("Missing the required parameter 'requiredBooleanGroup' when calling testGroupParameters", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // verify the required parameter 'requiredInt64Group' is set
        if (requiredInt64Group == null) {
            throw new RestClientResponseException("Missing the required parameter 'requiredInt64Group' when calling testGroupParameters", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "required_string_group", requiredStringGroup));
        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "required_int64_group", requiredInt64Group));
        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "string_group", stringGroup));
        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "int64_group", int64Group));
        

        if (requiredBooleanGroup != null)
        headerParams.add("required_boolean_group", apiClient.parameterToString(requiredBooleanGroup));
        if (booleanGroup != null)
        headerParams.add("boolean_group", apiClient.parameterToString(booleanGroup));
        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] { "bearer_test" };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/fake", HttpMethod.DELETE, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Fake endpoint to test group parameters (optional)
     * Fake endpoint to test group parameters (optional)
     * <p><b>400</b> - Something wrong
     * @param requiredStringGroup Required String in group parameters
     * @param requiredBooleanGroup Required Boolean in group parameters
     * @param requiredInt64Group Required Integer in group parameters
     * @param stringGroup String in group parameters
     * @param booleanGroup Boolean in group parameters
     * @param int64Group Integer in group parameters
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public void testGroupParameters(Integer requiredStringGroup, Boolean requiredBooleanGroup, Long requiredInt64Group, Integer stringGroup, Boolean booleanGroup, Long int64Group) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        testGroupParametersRequestCreation(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, stringGroup, booleanGroup, int64Group).body(localVarReturnType);
    }

    /**
     * Fake endpoint to test group parameters (optional)
     * Fake endpoint to test group parameters (optional)
     * <p><b>400</b> - Something wrong
     * @param requiredStringGroup Required String in group parameters
     * @param requiredBooleanGroup Required Boolean in group parameters
     * @param requiredInt64Group Required Integer in group parameters
     * @param stringGroup String in group parameters
     * @param booleanGroup Boolean in group parameters
     * @param int64Group Integer in group parameters
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> testGroupParametersWithHttpInfo(Integer requiredStringGroup, Boolean requiredBooleanGroup, Long requiredInt64Group, Integer stringGroup, Boolean booleanGroup, Long int64Group) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testGroupParametersRequestCreation(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, stringGroup, booleanGroup, int64Group).toEntity(localVarReturnType);
    }

    /**
     * Fake endpoint to test group parameters (optional)
     * Fake endpoint to test group parameters (optional)
     * <p><b>400</b> - Something wrong
     * @param requiredStringGroup Required String in group parameters
     * @param requiredBooleanGroup Required Boolean in group parameters
     * @param requiredInt64Group Required Integer in group parameters
     * @param stringGroup String in group parameters
     * @param booleanGroup Boolean in group parameters
     * @param int64Group Integer in group parameters
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testGroupParametersWithResponseSpec(Integer requiredStringGroup, Boolean requiredBooleanGroup, Long requiredInt64Group, Integer stringGroup, Boolean booleanGroup, Long int64Group) throws RestClientResponseException {
        return testGroupParametersRequestCreation(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, stringGroup, booleanGroup, int64Group);
    }
    /**
     * test inline additionalProperties
     * 
     * <p><b>200</b> - successful operation
     * @param requestBody request body
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testInlineAdditionalPropertiesRequestCreation(Map<String, String> requestBody) throws RestClientResponseException {
        Object postBody = requestBody;
        // verify the required parameter 'requestBody' is set
        if (requestBody == null) {
            throw new RestClientResponseException("Missing the required parameter 'requestBody' when calling testInlineAdditionalProperties", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/fake/inline-additionalProperties", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * test inline additionalProperties
     * 
     * <p><b>200</b> - successful operation
     * @param requestBody request body
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public void testInlineAdditionalProperties(Map<String, String> requestBody) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        testInlineAdditionalPropertiesRequestCreation(requestBody).body(localVarReturnType);
    }

    /**
     * test inline additionalProperties
     * 
     * <p><b>200</b> - successful operation
     * @param requestBody request body
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> testInlineAdditionalPropertiesWithHttpInfo(Map<String, String> requestBody) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testInlineAdditionalPropertiesRequestCreation(requestBody).toEntity(localVarReturnType);
    }

    /**
     * test inline additionalProperties
     * 
     * <p><b>200</b> - successful operation
     * @param requestBody request body
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testInlineAdditionalPropertiesWithResponseSpec(Map<String, String> requestBody) throws RestClientResponseException {
        return testInlineAdditionalPropertiesRequestCreation(requestBody);
    }
    /**
     * test inline free-form additionalProperties
     * 
     * <p><b>200</b> - successful operation
     * @param testInlineFreeformAdditionalPropertiesRequest request body
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testInlineFreeformAdditionalPropertiesRequestCreation(TestInlineFreeformAdditionalPropertiesRequest testInlineFreeformAdditionalPropertiesRequest) throws RestClientResponseException {
        Object postBody = testInlineFreeformAdditionalPropertiesRequest;
        // verify the required parameter 'testInlineFreeformAdditionalPropertiesRequest' is set
        if (testInlineFreeformAdditionalPropertiesRequest == null) {
            throw new RestClientResponseException("Missing the required parameter 'testInlineFreeformAdditionalPropertiesRequest' when calling testInlineFreeformAdditionalProperties", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/fake/inline-freeform-additionalProperties", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * test inline free-form additionalProperties
     * 
     * <p><b>200</b> - successful operation
     * @param testInlineFreeformAdditionalPropertiesRequest request body
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public void testInlineFreeformAdditionalProperties(TestInlineFreeformAdditionalPropertiesRequest testInlineFreeformAdditionalPropertiesRequest) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        testInlineFreeformAdditionalPropertiesRequestCreation(testInlineFreeformAdditionalPropertiesRequest).body(localVarReturnType);
    }

    /**
     * test inline free-form additionalProperties
     * 
     * <p><b>200</b> - successful operation
     * @param testInlineFreeformAdditionalPropertiesRequest request body
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> testInlineFreeformAdditionalPropertiesWithHttpInfo(TestInlineFreeformAdditionalPropertiesRequest testInlineFreeformAdditionalPropertiesRequest) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testInlineFreeformAdditionalPropertiesRequestCreation(testInlineFreeformAdditionalPropertiesRequest).toEntity(localVarReturnType);
    }

    /**
     * test inline free-form additionalProperties
     * 
     * <p><b>200</b> - successful operation
     * @param testInlineFreeformAdditionalPropertiesRequest request body
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testInlineFreeformAdditionalPropertiesWithResponseSpec(TestInlineFreeformAdditionalPropertiesRequest testInlineFreeformAdditionalPropertiesRequest) throws RestClientResponseException {
        return testInlineFreeformAdditionalPropertiesRequestCreation(testInlineFreeformAdditionalPropertiesRequest);
    }
    /**
     * test json serialization of form data
     * 
     * <p><b>200</b> - successful operation
     * @param param field1
     * @param param2 field2
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testJsonFormDataRequestCreation(String param, String param2) throws RestClientResponseException {
        Object postBody = null;
        // verify the required parameter 'param' is set
        if (param == null) {
            throw new RestClientResponseException("Missing the required parameter 'param' when calling testJsonFormData", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // verify the required parameter 'param2' is set
        if (param2 == null) {
            throw new RestClientResponseException("Missing the required parameter 'param2' when calling testJsonFormData", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        if (param != null)
            formParams.add("param", param);
        if (param2 != null)
            formParams.add("param2", param2);

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/x-www-form-urlencoded"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/fake/jsonFormData", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * test json serialization of form data
     * 
     * <p><b>200</b> - successful operation
     * @param param field1
     * @param param2 field2
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public void testJsonFormData(String param, String param2) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        testJsonFormDataRequestCreation(param, param2).body(localVarReturnType);
    }

    /**
     * test json serialization of form data
     * 
     * <p><b>200</b> - successful operation
     * @param param field1
     * @param param2 field2
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> testJsonFormDataWithHttpInfo(String param, String param2) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testJsonFormDataRequestCreation(param, param2).toEntity(localVarReturnType);
    }

    /**
     * test json serialization of form data
     * 
     * <p><b>200</b> - successful operation
     * @param param field1
     * @param param2 field2
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testJsonFormDataWithResponseSpec(String param, String param2) throws RestClientResponseException {
        return testJsonFormDataRequestCreation(param, param2);
    }
    /**
     * test nullable parent property
     * 
     * <p><b>200</b> - successful operation
     * @param childWithNullable request body
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testNullableRequestCreation(ChildWithNullable childWithNullable) throws RestClientResponseException {
        Object postBody = childWithNullable;
        // verify the required parameter 'childWithNullable' is set
        if (childWithNullable == null) {
            throw new RestClientResponseException("Missing the required parameter 'childWithNullable' when calling testNullable", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/fake/nullable", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * test nullable parent property
     * 
     * <p><b>200</b> - successful operation
     * @param childWithNullable request body
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public void testNullable(ChildWithNullable childWithNullable) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        testNullableRequestCreation(childWithNullable).body(localVarReturnType);
    }

    /**
     * test nullable parent property
     * 
     * <p><b>200</b> - successful operation
     * @param childWithNullable request body
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> testNullableWithHttpInfo(ChildWithNullable childWithNullable) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testNullableRequestCreation(childWithNullable).toEntity(localVarReturnType);
    }

    /**
     * test nullable parent property
     * 
     * <p><b>200</b> - successful operation
     * @param childWithNullable request body
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testNullableWithResponseSpec(ChildWithNullable childWithNullable) throws RestClientResponseException {
        return testNullableRequestCreation(childWithNullable);
    }
    /**
     * 
     * To test the collection format in query parameters
     * <p><b>200</b> - Success
     * @param pipe The pipe parameter
     * @param ioutil The ioutil parameter
     * @param http The http parameter
     * @param url The url parameter
     * @param context The context parameter
     * @param allowEmpty The allowEmpty parameter
     * @param language The language parameter
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testQueryParameterCollectionFormatRequestCreation(List<String> pipe, List<String> ioutil, List<String> http, List<String> url, List<String> context, String allowEmpty, Map<String, String> language) throws RestClientResponseException {
        Object postBody = null;
        // verify the required parameter 'pipe' is set
        if (pipe == null) {
            throw new RestClientResponseException("Missing the required parameter 'pipe' when calling testQueryParameterCollectionFormat", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // verify the required parameter 'ioutil' is set
        if (ioutil == null) {
            throw new RestClientResponseException("Missing the required parameter 'ioutil' when calling testQueryParameterCollectionFormat", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // verify the required parameter 'http' is set
        if (http == null) {
            throw new RestClientResponseException("Missing the required parameter 'http' when calling testQueryParameterCollectionFormat", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // verify the required parameter 'url' is set
        if (url == null) {
            throw new RestClientResponseException("Missing the required parameter 'url' when calling testQueryParameterCollectionFormat", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // verify the required parameter 'context' is set
        if (context == null) {
            throw new RestClientResponseException("Missing the required parameter 'context' when calling testQueryParameterCollectionFormat", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // verify the required parameter 'allowEmpty' is set
        if (allowEmpty == null) {
            throw new RestClientResponseException("Missing the required parameter 'allowEmpty' when calling testQueryParameterCollectionFormat", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        queryParams.putAll(apiClient.parameterToMultiValueMap(ApiClient.CollectionFormat.valueOf("pipes".toUpperCase(Locale.ROOT)), "pipe", pipe));
        queryParams.putAll(apiClient.parameterToMultiValueMap(ApiClient.CollectionFormat.valueOf("csv".toUpperCase(Locale.ROOT)), "ioutil", ioutil));
        queryParams.putAll(apiClient.parameterToMultiValueMap(ApiClient.CollectionFormat.valueOf("ssv".toUpperCase(Locale.ROOT)), "http", http));
        queryParams.putAll(apiClient.parameterToMultiValueMap(ApiClient.CollectionFormat.valueOf("csv".toUpperCase(Locale.ROOT)), "url", url));
        queryParams.putAll(apiClient.parameterToMultiValueMap(ApiClient.CollectionFormat.valueOf("multi".toUpperCase(Locale.ROOT)), "context", context));
        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "language", language));
        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "allowEmpty", allowEmpty));
        
        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/fake/test-query-parameters", HttpMethod.PUT, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * To test the collection format in query parameters
     * <p><b>200</b> - Success
     * @param pipe The pipe parameter
     * @param ioutil The ioutil parameter
     * @param http The http parameter
     * @param url The url parameter
     * @param context The context parameter
     * @param allowEmpty The allowEmpty parameter
     * @param language The language parameter
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public void testQueryParameterCollectionFormat(List<String> pipe, List<String> ioutil, List<String> http, List<String> url, List<String> context, String allowEmpty, Map<String, String> language) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        testQueryParameterCollectionFormatRequestCreation(pipe, ioutil, http, url, context, allowEmpty, language).body(localVarReturnType);
    }

    /**
     * 
     * To test the collection format in query parameters
     * <p><b>200</b> - Success
     * @param pipe The pipe parameter
     * @param ioutil The ioutil parameter
     * @param http The http parameter
     * @param url The url parameter
     * @param context The context parameter
     * @param allowEmpty The allowEmpty parameter
     * @param language The language parameter
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> testQueryParameterCollectionFormatWithHttpInfo(List<String> pipe, List<String> ioutil, List<String> http, List<String> url, List<String> context, String allowEmpty, Map<String, String> language) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testQueryParameterCollectionFormatRequestCreation(pipe, ioutil, http, url, context, allowEmpty, language).toEntity(localVarReturnType);
    }

    /**
     * 
     * To test the collection format in query parameters
     * <p><b>200</b> - Success
     * @param pipe The pipe parameter
     * @param ioutil The ioutil parameter
     * @param http The http parameter
     * @param url The url parameter
     * @param context The context parameter
     * @param allowEmpty The allowEmpty parameter
     * @param language The language parameter
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testQueryParameterCollectionFormatWithResponseSpec(List<String> pipe, List<String> ioutil, List<String> http, List<String> url, List<String> context, String allowEmpty, Map<String, String> language) throws RestClientResponseException {
        return testQueryParameterCollectionFormatRequestCreation(pipe, ioutil, http, url, context, allowEmpty, language);
    }
    /**
     * test referenced string map
     * 
     * <p><b>200</b> - successful operation
     * @param requestBody request body
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testStringMapReferenceRequestCreation(Map<String, String> requestBody) throws RestClientResponseException {
        Object postBody = requestBody;
        // verify the required parameter 'requestBody' is set
        if (requestBody == null) {
            throw new RestClientResponseException("Missing the required parameter 'requestBody' when calling testStringMapReference", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/fake/stringMap-reference", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * test referenced string map
     * 
     * <p><b>200</b> - successful operation
     * @param requestBody request body
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public void testStringMapReference(Map<String, String> requestBody) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        testStringMapReferenceRequestCreation(requestBody).body(localVarReturnType);
    }

    /**
     * test referenced string map
     * 
     * <p><b>200</b> - successful operation
     * @param requestBody request body
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> testStringMapReferenceWithHttpInfo(Map<String, String> requestBody) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testStringMapReferenceRequestCreation(requestBody).toEntity(localVarReturnType);
    }

    /**
     * test referenced string map
     * 
     * <p><b>200</b> - successful operation
     * @param requestBody request body
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testStringMapReferenceWithResponseSpec(Map<String, String> requestBody) throws RestClientResponseException {
        return testStringMapReferenceRequestCreation(requestBody);
    }
}
