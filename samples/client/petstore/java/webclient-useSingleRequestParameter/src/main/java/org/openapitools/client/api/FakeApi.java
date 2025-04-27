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
import java.util.Objects;
import java.util.Arrays;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.FileSystemResource;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.reactive.function.client.WebClient.ResponseSpec;
import org.springframework.web.reactive.function.client.WebClientResponseException;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Flux;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen", comments = "Generator version: 7.14.0-SNAPSHOT")
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
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec fakeBigDecimalMapRequestCreation() throws WebClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = { 
            "*/*"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<FakeBigDecimalMap200Response> localVarReturnType = new ParameterizedTypeReference<FakeBigDecimalMap200Response>() {};
        return apiClient.invokeAPI("/fake/BigDecimalMap", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * for Java apache and Java native, test toUrlQueryString for maps with BegDecimal keys
     * <p><b>200</b> - successful operation
     * @return FakeBigDecimalMap200Response
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<FakeBigDecimalMap200Response> fakeBigDecimalMap() throws WebClientResponseException {
        ParameterizedTypeReference<FakeBigDecimalMap200Response> localVarReturnType = new ParameterizedTypeReference<FakeBigDecimalMap200Response>() {};
        return fakeBigDecimalMapRequestCreation().bodyToMono(localVarReturnType);
    }

    /**
     * 
     * for Java apache and Java native, test toUrlQueryString for maps with BegDecimal keys
     * <p><b>200</b> - successful operation
     * @return ResponseEntity&lt;FakeBigDecimalMap200Response&gt;
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<FakeBigDecimalMap200Response>> fakeBigDecimalMapWithHttpInfo() throws WebClientResponseException {
        ParameterizedTypeReference<FakeBigDecimalMap200Response> localVarReturnType = new ParameterizedTypeReference<FakeBigDecimalMap200Response>() {};
        return fakeBigDecimalMapRequestCreation().toEntity(localVarReturnType);
    }

    /**
     * 
     * for Java apache and Java native, test toUrlQueryString for maps with BegDecimal keys
     * <p><b>200</b> - successful operation
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec fakeBigDecimalMapWithResponseSpec() throws WebClientResponseException {
        return fakeBigDecimalMapRequestCreation();
    }

    /**
     * Health check endpoint
     * 
     * <p><b>200</b> - The instance started successfully
     * @return HealthCheckResult
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec fakeHealthGetRequestCreation() throws WebClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = { 
            "application/json"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<HealthCheckResult> localVarReturnType = new ParameterizedTypeReference<HealthCheckResult>() {};
        return apiClient.invokeAPI("/fake/health", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Health check endpoint
     * 
     * <p><b>200</b> - The instance started successfully
     * @return HealthCheckResult
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<HealthCheckResult> fakeHealthGet() throws WebClientResponseException {
        ParameterizedTypeReference<HealthCheckResult> localVarReturnType = new ParameterizedTypeReference<HealthCheckResult>() {};
        return fakeHealthGetRequestCreation().bodyToMono(localVarReturnType);
    }

    /**
     * Health check endpoint
     * 
     * <p><b>200</b> - The instance started successfully
     * @return ResponseEntity&lt;HealthCheckResult&gt;
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<HealthCheckResult>> fakeHealthGetWithHttpInfo() throws WebClientResponseException {
        ParameterizedTypeReference<HealthCheckResult> localVarReturnType = new ParameterizedTypeReference<HealthCheckResult>() {};
        return fakeHealthGetRequestCreation().toEntity(localVarReturnType);
    }

    /**
     * Health check endpoint
     * 
     * <p><b>200</b> - The instance started successfully
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec fakeHealthGetWithResponseSpec() throws WebClientResponseException {
        return fakeHealthGetRequestCreation();
    }

    public class FakeHttpSignatureTestRequest {
        private @javax.annotation.Nonnull Pet pet;
        private @javax.annotation.Nullable String query1;
        private @javax.annotation.Nullable String header1;

        public FakeHttpSignatureTestRequest() {}

        public FakeHttpSignatureTestRequest(@javax.annotation.Nonnull Pet pet, @javax.annotation.Nullable String query1, @javax.annotation.Nullable String header1) {
            this.pet = pet;
            this.query1 = query1;
            this.header1 = header1;
        }

        public @javax.annotation.Nonnull Pet pet() {
            return this.pet;
        }
        public FakeHttpSignatureTestRequest pet(@javax.annotation.Nonnull Pet pet) {
            this.pet = pet;
            return this;
        }

        public @javax.annotation.Nullable String query1() {
            return this.query1;
        }
        public FakeHttpSignatureTestRequest query1(@javax.annotation.Nullable String query1) {
            this.query1 = query1;
            return this;
        }

        public @javax.annotation.Nullable String header1() {
            return this.header1;
        }
        public FakeHttpSignatureTestRequest header1(@javax.annotation.Nullable String header1) {
            this.header1 = header1;
            return this;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) {
                return true;
            }
            if (o == null || getClass() != o.getClass()) {
                return false;
            }
            FakeHttpSignatureTestRequest request = (FakeHttpSignatureTestRequest) o;
            return Objects.equals(this.pet, request.pet()) &&
                Objects.equals(this.query1, request.query1()) &&
                Objects.equals(this.header1, request.header1());
        }

        @Override
        public int hashCode() {
            return Objects.hash(pet, query1, header1);
        }
    }

    /**
     * test http signature authentication
     * 
     * <p><b>200</b> - The instance started successfully
     * @param requestParameters The fakeHttpSignatureTest request parameters as object
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Void> fakeHttpSignatureTest(FakeHttpSignatureTestRequest requestParameters) throws WebClientResponseException {
        return this.fakeHttpSignatureTest(requestParameters.pet(), requestParameters.query1(), requestParameters.header1());
    }

    /**
     * test http signature authentication
     * 
     * <p><b>200</b> - The instance started successfully
     * @param requestParameters The fakeHttpSignatureTest request parameters as object
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<Void>> fakeHttpSignatureTestWithHttpInfo(FakeHttpSignatureTestRequest requestParameters) throws WebClientResponseException {
        return this.fakeHttpSignatureTestWithHttpInfo(requestParameters.pet(), requestParameters.query1(), requestParameters.header1());
    }

    /**
     * test http signature authentication
     * 
     * <p><b>200</b> - The instance started successfully
     * @param requestParameters The fakeHttpSignatureTest request parameters as object
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec fakeHttpSignatureTestWithResponseSpec(FakeHttpSignatureTestRequest requestParameters) throws WebClientResponseException {
       return this.fakeHttpSignatureTestWithResponseSpec(requestParameters.pet(), requestParameters.query1(), requestParameters.header1());
    }

          
    /**
     * test http signature authentication
     * 
     * <p><b>200</b> - The instance started successfully
     * @param pet Pet object that needs to be added to the store
     * @param query1 query parameter
     * @param header1 header parameter
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec fakeHttpSignatureTestRequestCreation(@javax.annotation.Nonnull Pet pet, @javax.annotation.Nullable String query1, @javax.annotation.Nullable String header1) throws WebClientResponseException {
        Object postBody = pet;
        // verify the required parameter 'pet' is set
        if (pet == null) {
            throw new WebClientResponseException("Missing the required parameter 'pet' when calling fakeHttpSignatureTest", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

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

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return apiClient.invokeAPI("/fake/http-signature-test", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * test http signature authentication
     * 
     * <p><b>200</b> - The instance started successfully
     * @param pet Pet object that needs to be added to the store
     * @param query1 query parameter
     * @param header1 header parameter
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Void> fakeHttpSignatureTest(@javax.annotation.Nonnull Pet pet, @javax.annotation.Nullable String query1, @javax.annotation.Nullable String header1) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return fakeHttpSignatureTestRequestCreation(pet, query1, header1).bodyToMono(localVarReturnType);
    }

    /**
     * test http signature authentication
     * 
     * <p><b>200</b> - The instance started successfully
     * @param pet Pet object that needs to be added to the store
     * @param query1 query parameter
     * @param header1 header parameter
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<Void>> fakeHttpSignatureTestWithHttpInfo(@javax.annotation.Nonnull Pet pet, @javax.annotation.Nullable String query1, @javax.annotation.Nullable String header1) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
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
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec fakeHttpSignatureTestWithResponseSpec(@javax.annotation.Nonnull Pet pet, @javax.annotation.Nullable String query1, @javax.annotation.Nullable String header1) throws WebClientResponseException {
        return fakeHttpSignatureTestRequestCreation(pet, query1, header1);
    }

    /**
     * 
     * Test serialization of outer boolean types
     * <p><b>200</b> - Output boolean
     * @param body Input boolean as post body
     * @return Boolean
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec fakeOuterBooleanSerializeRequestCreation(@javax.annotation.Nullable Boolean body) throws WebClientResponseException {
        Object postBody = body;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = { 
            "*/*"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Boolean> localVarReturnType = new ParameterizedTypeReference<Boolean>() {};
        return apiClient.invokeAPI("/fake/outer/boolean", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * Test serialization of outer boolean types
     * <p><b>200</b> - Output boolean
     * @param body Input boolean as post body
     * @return Boolean
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Boolean> fakeOuterBooleanSerialize(@javax.annotation.Nullable Boolean body) throws WebClientResponseException {
        ParameterizedTypeReference<Boolean> localVarReturnType = new ParameterizedTypeReference<Boolean>() {};
        return fakeOuterBooleanSerializeRequestCreation(body).bodyToMono(localVarReturnType);
    }

    /**
     * 
     * Test serialization of outer boolean types
     * <p><b>200</b> - Output boolean
     * @param body Input boolean as post body
     * @return ResponseEntity&lt;Boolean&gt;
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<Boolean>> fakeOuterBooleanSerializeWithHttpInfo(@javax.annotation.Nullable Boolean body) throws WebClientResponseException {
        ParameterizedTypeReference<Boolean> localVarReturnType = new ParameterizedTypeReference<Boolean>() {};
        return fakeOuterBooleanSerializeRequestCreation(body).toEntity(localVarReturnType);
    }

    /**
     * 
     * Test serialization of outer boolean types
     * <p><b>200</b> - Output boolean
     * @param body Input boolean as post body
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec fakeOuterBooleanSerializeWithResponseSpec(@javax.annotation.Nullable Boolean body) throws WebClientResponseException {
        return fakeOuterBooleanSerializeRequestCreation(body);
    }

    /**
     * 
     * Test serialization of object with outer number type
     * <p><b>200</b> - Output composite
     * @param outerComposite Input composite as post body
     * @return OuterComposite
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec fakeOuterCompositeSerializeRequestCreation(@javax.annotation.Nullable OuterComposite outerComposite) throws WebClientResponseException {
        Object postBody = outerComposite;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = { 
            "*/*"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<OuterComposite> localVarReturnType = new ParameterizedTypeReference<OuterComposite>() {};
        return apiClient.invokeAPI("/fake/outer/composite", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * Test serialization of object with outer number type
     * <p><b>200</b> - Output composite
     * @param outerComposite Input composite as post body
     * @return OuterComposite
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<OuterComposite> fakeOuterCompositeSerialize(@javax.annotation.Nullable OuterComposite outerComposite) throws WebClientResponseException {
        ParameterizedTypeReference<OuterComposite> localVarReturnType = new ParameterizedTypeReference<OuterComposite>() {};
        return fakeOuterCompositeSerializeRequestCreation(outerComposite).bodyToMono(localVarReturnType);
    }

    /**
     * 
     * Test serialization of object with outer number type
     * <p><b>200</b> - Output composite
     * @param outerComposite Input composite as post body
     * @return ResponseEntity&lt;OuterComposite&gt;
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<OuterComposite>> fakeOuterCompositeSerializeWithHttpInfo(@javax.annotation.Nullable OuterComposite outerComposite) throws WebClientResponseException {
        ParameterizedTypeReference<OuterComposite> localVarReturnType = new ParameterizedTypeReference<OuterComposite>() {};
        return fakeOuterCompositeSerializeRequestCreation(outerComposite).toEntity(localVarReturnType);
    }

    /**
     * 
     * Test serialization of object with outer number type
     * <p><b>200</b> - Output composite
     * @param outerComposite Input composite as post body
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec fakeOuterCompositeSerializeWithResponseSpec(@javax.annotation.Nullable OuterComposite outerComposite) throws WebClientResponseException {
        return fakeOuterCompositeSerializeRequestCreation(outerComposite);
    }

    /**
     * 
     * Test serialization of outer number types
     * <p><b>200</b> - Output number
     * @param body Input number as post body
     * @return BigDecimal
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec fakeOuterNumberSerializeRequestCreation(@javax.annotation.Nullable BigDecimal body) throws WebClientResponseException {
        Object postBody = body;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = { 
            "*/*"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<BigDecimal> localVarReturnType = new ParameterizedTypeReference<BigDecimal>() {};
        return apiClient.invokeAPI("/fake/outer/number", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * Test serialization of outer number types
     * <p><b>200</b> - Output number
     * @param body Input number as post body
     * @return BigDecimal
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<BigDecimal> fakeOuterNumberSerialize(@javax.annotation.Nullable BigDecimal body) throws WebClientResponseException {
        ParameterizedTypeReference<BigDecimal> localVarReturnType = new ParameterizedTypeReference<BigDecimal>() {};
        return fakeOuterNumberSerializeRequestCreation(body).bodyToMono(localVarReturnType);
    }

    /**
     * 
     * Test serialization of outer number types
     * <p><b>200</b> - Output number
     * @param body Input number as post body
     * @return ResponseEntity&lt;BigDecimal&gt;
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<BigDecimal>> fakeOuterNumberSerializeWithHttpInfo(@javax.annotation.Nullable BigDecimal body) throws WebClientResponseException {
        ParameterizedTypeReference<BigDecimal> localVarReturnType = new ParameterizedTypeReference<BigDecimal>() {};
        return fakeOuterNumberSerializeRequestCreation(body).toEntity(localVarReturnType);
    }

    /**
     * 
     * Test serialization of outer number types
     * <p><b>200</b> - Output number
     * @param body Input number as post body
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec fakeOuterNumberSerializeWithResponseSpec(@javax.annotation.Nullable BigDecimal body) throws WebClientResponseException {
        return fakeOuterNumberSerializeRequestCreation(body);
    }

    /**
     * 
     * Test serialization of outer string types
     * <p><b>200</b> - Output string
     * @param body Input string as post body
     * @return String
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec fakeOuterStringSerializeRequestCreation(@javax.annotation.Nullable String body) throws WebClientResponseException {
        Object postBody = body;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = { 
            "*/*"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<String>() {};
        return apiClient.invokeAPI("/fake/outer/string", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * Test serialization of outer string types
     * <p><b>200</b> - Output string
     * @param body Input string as post body
     * @return String
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<String> fakeOuterStringSerialize(@javax.annotation.Nullable String body) throws WebClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<String>() {};
        return fakeOuterStringSerializeRequestCreation(body).bodyToMono(localVarReturnType);
    }

    /**
     * 
     * Test serialization of outer string types
     * <p><b>200</b> - Output string
     * @param body Input string as post body
     * @return ResponseEntity&lt;String&gt;
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<String>> fakeOuterStringSerializeWithHttpInfo(@javax.annotation.Nullable String body) throws WebClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<String>() {};
        return fakeOuterStringSerializeRequestCreation(body).toEntity(localVarReturnType);
    }

    /**
     * 
     * Test serialization of outer string types
     * <p><b>200</b> - Output string
     * @param body Input string as post body
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec fakeOuterStringSerializeWithResponseSpec(@javax.annotation.Nullable String body) throws WebClientResponseException {
        return fakeOuterStringSerializeRequestCreation(body);
    }

    /**
     * 
     * Test serialization of enum (int) properties with examples
     * <p><b>200</b> - Output enum (int)
     * @param outerObjectWithEnumProperty Input enum (int) as post body
     * @return OuterObjectWithEnumProperty
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec fakePropertyEnumIntegerSerializeRequestCreation(@javax.annotation.Nonnull OuterObjectWithEnumProperty outerObjectWithEnumProperty) throws WebClientResponseException {
        Object postBody = outerObjectWithEnumProperty;
        // verify the required parameter 'outerObjectWithEnumProperty' is set
        if (outerObjectWithEnumProperty == null) {
            throw new WebClientResponseException("Missing the required parameter 'outerObjectWithEnumProperty' when calling fakePropertyEnumIntegerSerialize", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = { 
            "*/*"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<OuterObjectWithEnumProperty> localVarReturnType = new ParameterizedTypeReference<OuterObjectWithEnumProperty>() {};
        return apiClient.invokeAPI("/fake/property/enum-int", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * Test serialization of enum (int) properties with examples
     * <p><b>200</b> - Output enum (int)
     * @param outerObjectWithEnumProperty Input enum (int) as post body
     * @return OuterObjectWithEnumProperty
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<OuterObjectWithEnumProperty> fakePropertyEnumIntegerSerialize(@javax.annotation.Nonnull OuterObjectWithEnumProperty outerObjectWithEnumProperty) throws WebClientResponseException {
        ParameterizedTypeReference<OuterObjectWithEnumProperty> localVarReturnType = new ParameterizedTypeReference<OuterObjectWithEnumProperty>() {};
        return fakePropertyEnumIntegerSerializeRequestCreation(outerObjectWithEnumProperty).bodyToMono(localVarReturnType);
    }

    /**
     * 
     * Test serialization of enum (int) properties with examples
     * <p><b>200</b> - Output enum (int)
     * @param outerObjectWithEnumProperty Input enum (int) as post body
     * @return ResponseEntity&lt;OuterObjectWithEnumProperty&gt;
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<OuterObjectWithEnumProperty>> fakePropertyEnumIntegerSerializeWithHttpInfo(@javax.annotation.Nonnull OuterObjectWithEnumProperty outerObjectWithEnumProperty) throws WebClientResponseException {
        ParameterizedTypeReference<OuterObjectWithEnumProperty> localVarReturnType = new ParameterizedTypeReference<OuterObjectWithEnumProperty>() {};
        return fakePropertyEnumIntegerSerializeRequestCreation(outerObjectWithEnumProperty).toEntity(localVarReturnType);
    }

    /**
     * 
     * Test serialization of enum (int) properties with examples
     * <p><b>200</b> - Output enum (int)
     * @param outerObjectWithEnumProperty Input enum (int) as post body
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec fakePropertyEnumIntegerSerializeWithResponseSpec(@javax.annotation.Nonnull OuterObjectWithEnumProperty outerObjectWithEnumProperty) throws WebClientResponseException {
        return fakePropertyEnumIntegerSerializeRequestCreation(outerObjectWithEnumProperty);
    }

    /**
     * test referenced additionalProperties
     * 
     * <p><b>200</b> - successful operation
     * @param requestBody request body
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testAdditionalPropertiesReferenceRequestCreation(@javax.annotation.Nonnull Map<String, Object> requestBody) throws WebClientResponseException {
        Object postBody = requestBody;
        // verify the required parameter 'requestBody' is set
        if (requestBody == null) {
            throw new WebClientResponseException("Missing the required parameter 'requestBody' when calling testAdditionalPropertiesReference", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return apiClient.invokeAPI("/fake/additionalProperties-reference", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * test referenced additionalProperties
     * 
     * <p><b>200</b> - successful operation
     * @param requestBody request body
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Void> testAdditionalPropertiesReference(@javax.annotation.Nonnull Map<String, Object> requestBody) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return testAdditionalPropertiesReferenceRequestCreation(requestBody).bodyToMono(localVarReturnType);
    }

    /**
     * test referenced additionalProperties
     * 
     * <p><b>200</b> - successful operation
     * @param requestBody request body
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<Void>> testAdditionalPropertiesReferenceWithHttpInfo(@javax.annotation.Nonnull Map<String, Object> requestBody) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return testAdditionalPropertiesReferenceRequestCreation(requestBody).toEntity(localVarReturnType);
    }

    /**
     * test referenced additionalProperties
     * 
     * <p><b>200</b> - successful operation
     * @param requestBody request body
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testAdditionalPropertiesReferenceWithResponseSpec(@javax.annotation.Nonnull Map<String, Object> requestBody) throws WebClientResponseException {
        return testAdditionalPropertiesReferenceRequestCreation(requestBody);
    }

    /**
     * 
     * For this test, the body has to be a binary file.
     * <p><b>200</b> - Success
     * @param body image to upload
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testBodyWithBinaryRequestCreation(@javax.annotation.Nullable File body) throws WebClientResponseException {
        Object postBody = body;
        // verify the required parameter 'body' is set
        if (body == null) {
            throw new WebClientResponseException("Missing the required parameter 'body' when calling testBodyWithBinary", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "image/png"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return apiClient.invokeAPI("/fake/body-with-binary", HttpMethod.PUT, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * For this test, the body has to be a binary file.
     * <p><b>200</b> - Success
     * @param body image to upload
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Void> testBodyWithBinary(@javax.annotation.Nullable File body) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return testBodyWithBinaryRequestCreation(body).bodyToMono(localVarReturnType);
    }

    /**
     * 
     * For this test, the body has to be a binary file.
     * <p><b>200</b> - Success
     * @param body image to upload
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<Void>> testBodyWithBinaryWithHttpInfo(@javax.annotation.Nullable File body) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return testBodyWithBinaryRequestCreation(body).toEntity(localVarReturnType);
    }

    /**
     * 
     * For this test, the body has to be a binary file.
     * <p><b>200</b> - Success
     * @param body image to upload
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testBodyWithBinaryWithResponseSpec(@javax.annotation.Nullable File body) throws WebClientResponseException {
        return testBodyWithBinaryRequestCreation(body);
    }

    /**
     * 
     * For this test, the body for this request must reference a schema named &#x60;File&#x60;.
     * <p><b>200</b> - Success
     * @param fileSchemaTestClass The fileSchemaTestClass parameter
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testBodyWithFileSchemaRequestCreation(@javax.annotation.Nonnull FileSchemaTestClass fileSchemaTestClass) throws WebClientResponseException {
        Object postBody = fileSchemaTestClass;
        // verify the required parameter 'fileSchemaTestClass' is set
        if (fileSchemaTestClass == null) {
            throw new WebClientResponseException("Missing the required parameter 'fileSchemaTestClass' when calling testBodyWithFileSchema", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return apiClient.invokeAPI("/fake/body-with-file-schema", HttpMethod.PUT, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * For this test, the body for this request must reference a schema named &#x60;File&#x60;.
     * <p><b>200</b> - Success
     * @param fileSchemaTestClass The fileSchemaTestClass parameter
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Void> testBodyWithFileSchema(@javax.annotation.Nonnull FileSchemaTestClass fileSchemaTestClass) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return testBodyWithFileSchemaRequestCreation(fileSchemaTestClass).bodyToMono(localVarReturnType);
    }

    /**
     * 
     * For this test, the body for this request must reference a schema named &#x60;File&#x60;.
     * <p><b>200</b> - Success
     * @param fileSchemaTestClass The fileSchemaTestClass parameter
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<Void>> testBodyWithFileSchemaWithHttpInfo(@javax.annotation.Nonnull FileSchemaTestClass fileSchemaTestClass) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return testBodyWithFileSchemaRequestCreation(fileSchemaTestClass).toEntity(localVarReturnType);
    }

    /**
     * 
     * For this test, the body for this request must reference a schema named &#x60;File&#x60;.
     * <p><b>200</b> - Success
     * @param fileSchemaTestClass The fileSchemaTestClass parameter
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testBodyWithFileSchemaWithResponseSpec(@javax.annotation.Nonnull FileSchemaTestClass fileSchemaTestClass) throws WebClientResponseException {
        return testBodyWithFileSchemaRequestCreation(fileSchemaTestClass);
    }

    public class TestBodyWithQueryParamsRequest {
        private @javax.annotation.Nonnull String query;
        private @javax.annotation.Nonnull User user;

        public TestBodyWithQueryParamsRequest() {}

        public TestBodyWithQueryParamsRequest(@javax.annotation.Nonnull String query, @javax.annotation.Nonnull User user) {
            this.query = query;
            this.user = user;
        }

        public @javax.annotation.Nonnull String query() {
            return this.query;
        }
        public TestBodyWithQueryParamsRequest query(@javax.annotation.Nonnull String query) {
            this.query = query;
            return this;
        }

        public @javax.annotation.Nonnull User user() {
            return this.user;
        }
        public TestBodyWithQueryParamsRequest user(@javax.annotation.Nonnull User user) {
            this.user = user;
            return this;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) {
                return true;
            }
            if (o == null || getClass() != o.getClass()) {
                return false;
            }
            TestBodyWithQueryParamsRequest request = (TestBodyWithQueryParamsRequest) o;
            return Objects.equals(this.query, request.query()) &&
                Objects.equals(this.user, request.user());
        }

        @Override
        public int hashCode() {
            return Objects.hash(query, user);
        }
    }

    /**
     * 
     * 
     * <p><b>200</b> - Success
     * @param requestParameters The testBodyWithQueryParams request parameters as object
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Void> testBodyWithQueryParams(TestBodyWithQueryParamsRequest requestParameters) throws WebClientResponseException {
        return this.testBodyWithQueryParams(requestParameters.query(), requestParameters.user());
    }

    /**
     * 
     * 
     * <p><b>200</b> - Success
     * @param requestParameters The testBodyWithQueryParams request parameters as object
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<Void>> testBodyWithQueryParamsWithHttpInfo(TestBodyWithQueryParamsRequest requestParameters) throws WebClientResponseException {
        return this.testBodyWithQueryParamsWithHttpInfo(requestParameters.query(), requestParameters.user());
    }

    /**
     * 
     * 
     * <p><b>200</b> - Success
     * @param requestParameters The testBodyWithQueryParams request parameters as object
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testBodyWithQueryParamsWithResponseSpec(TestBodyWithQueryParamsRequest requestParameters) throws WebClientResponseException {
       return this.testBodyWithQueryParamsWithResponseSpec(requestParameters.query(), requestParameters.user());
    }

          
    /**
     * 
     * 
     * <p><b>200</b> - Success
     * @param query The query parameter
     * @param user The user parameter
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testBodyWithQueryParamsRequestCreation(@javax.annotation.Nonnull String query, @javax.annotation.Nonnull User user) throws WebClientResponseException {
        Object postBody = user;
        // verify the required parameter 'query' is set
        if (query == null) {
            throw new WebClientResponseException("Missing the required parameter 'query' when calling testBodyWithQueryParams", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // verify the required parameter 'user' is set
        if (user == null) {
            throw new WebClientResponseException("Missing the required parameter 'user' when calling testBodyWithQueryParams", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "query", query));
        
        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return apiClient.invokeAPI("/fake/body-with-query-params", HttpMethod.PUT, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - Success
     * @param query The query parameter
     * @param user The user parameter
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Void> testBodyWithQueryParams(@javax.annotation.Nonnull String query, @javax.annotation.Nonnull User user) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return testBodyWithQueryParamsRequestCreation(query, user).bodyToMono(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - Success
     * @param query The query parameter
     * @param user The user parameter
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<Void>> testBodyWithQueryParamsWithHttpInfo(@javax.annotation.Nonnull String query, @javax.annotation.Nonnull User user) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return testBodyWithQueryParamsRequestCreation(query, user).toEntity(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - Success
     * @param query The query parameter
     * @param user The user parameter
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testBodyWithQueryParamsWithResponseSpec(@javax.annotation.Nonnull String query, @javax.annotation.Nonnull User user) throws WebClientResponseException {
        return testBodyWithQueryParamsRequestCreation(query, user);
    }

    /**
     * To test \&quot;client\&quot; model
     * To test \&quot;client\&quot; model
     * <p><b>200</b> - successful operation
     * @param client client model
     * @return Client
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testClientModelRequestCreation(@javax.annotation.Nonnull Client client) throws WebClientResponseException {
        Object postBody = client;
        // verify the required parameter 'client' is set
        if (client == null) {
            throw new WebClientResponseException("Missing the required parameter 'client' when calling testClientModel", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = { 
            "application/json"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Client> localVarReturnType = new ParameterizedTypeReference<Client>() {};
        return apiClient.invokeAPI("/fake", HttpMethod.PATCH, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * To test \&quot;client\&quot; model
     * To test \&quot;client\&quot; model
     * <p><b>200</b> - successful operation
     * @param client client model
     * @return Client
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Client> testClientModel(@javax.annotation.Nonnull Client client) throws WebClientResponseException {
        ParameterizedTypeReference<Client> localVarReturnType = new ParameterizedTypeReference<Client>() {};
        return testClientModelRequestCreation(client).bodyToMono(localVarReturnType);
    }

    /**
     * To test \&quot;client\&quot; model
     * To test \&quot;client\&quot; model
     * <p><b>200</b> - successful operation
     * @param client client model
     * @return ResponseEntity&lt;Client&gt;
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<Client>> testClientModelWithHttpInfo(@javax.annotation.Nonnull Client client) throws WebClientResponseException {
        ParameterizedTypeReference<Client> localVarReturnType = new ParameterizedTypeReference<Client>() {};
        return testClientModelRequestCreation(client).toEntity(localVarReturnType);
    }

    /**
     * To test \&quot;client\&quot; model
     * To test \&quot;client\&quot; model
     * <p><b>200</b> - successful operation
     * @param client client model
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testClientModelWithResponseSpec(@javax.annotation.Nonnull Client client) throws WebClientResponseException {
        return testClientModelRequestCreation(client);
    }

    public class TestEndpointParametersRequest {
        private @javax.annotation.Nonnull BigDecimal number;
        private @javax.annotation.Nonnull Double _double;
        private @javax.annotation.Nonnull String patternWithoutDelimiter;
        private @javax.annotation.Nonnull byte[] _byte;
        private @javax.annotation.Nullable Integer integer;
        private @javax.annotation.Nullable Integer int32;
        private @javax.annotation.Nullable Long int64;
        private @javax.annotation.Nullable Float _float;
        private @javax.annotation.Nullable String string;
        private @javax.annotation.Nullable File binary;
        private @javax.annotation.Nullable LocalDate date;
        private @javax.annotation.Nullable OffsetDateTime dateTime;
        private @javax.annotation.Nullable String password;
        private @javax.annotation.Nullable String paramCallback;

        public TestEndpointParametersRequest() {}

        public TestEndpointParametersRequest(@javax.annotation.Nonnull BigDecimal number, @javax.annotation.Nonnull Double _double, @javax.annotation.Nonnull String patternWithoutDelimiter, @javax.annotation.Nonnull byte[] _byte, @javax.annotation.Nullable Integer integer, @javax.annotation.Nullable Integer int32, @javax.annotation.Nullable Long int64, @javax.annotation.Nullable Float _float, @javax.annotation.Nullable String string, @javax.annotation.Nullable File binary, @javax.annotation.Nullable LocalDate date, @javax.annotation.Nullable OffsetDateTime dateTime, @javax.annotation.Nullable String password, @javax.annotation.Nullable String paramCallback) {
            this.number = number;
            this._double = _double;
            this.patternWithoutDelimiter = patternWithoutDelimiter;
            this._byte = _byte;
            this.integer = integer;
            this.int32 = int32;
            this.int64 = int64;
            this._float = _float;
            this.string = string;
            this.binary = binary;
            this.date = date;
            this.dateTime = dateTime;
            this.password = password;
            this.paramCallback = paramCallback;
        }

        public @javax.annotation.Nonnull BigDecimal number() {
            return this.number;
        }
        public TestEndpointParametersRequest number(@javax.annotation.Nonnull BigDecimal number) {
            this.number = number;
            return this;
        }

        public @javax.annotation.Nonnull Double _double() {
            return this._double;
        }
        public TestEndpointParametersRequest _double(@javax.annotation.Nonnull Double _double) {
            this._double = _double;
            return this;
        }

        public @javax.annotation.Nonnull String patternWithoutDelimiter() {
            return this.patternWithoutDelimiter;
        }
        public TestEndpointParametersRequest patternWithoutDelimiter(@javax.annotation.Nonnull String patternWithoutDelimiter) {
            this.patternWithoutDelimiter = patternWithoutDelimiter;
            return this;
        }

        public @javax.annotation.Nonnull byte[] _byte() {
            return this._byte;
        }
        public TestEndpointParametersRequest _byte(@javax.annotation.Nonnull byte[] _byte) {
            this._byte = _byte;
            return this;
        }

        public @javax.annotation.Nullable Integer integer() {
            return this.integer;
        }
        public TestEndpointParametersRequest integer(@javax.annotation.Nullable Integer integer) {
            this.integer = integer;
            return this;
        }

        public @javax.annotation.Nullable Integer int32() {
            return this.int32;
        }
        public TestEndpointParametersRequest int32(@javax.annotation.Nullable Integer int32) {
            this.int32 = int32;
            return this;
        }

        public @javax.annotation.Nullable Long int64() {
            return this.int64;
        }
        public TestEndpointParametersRequest int64(@javax.annotation.Nullable Long int64) {
            this.int64 = int64;
            return this;
        }

        public @javax.annotation.Nullable Float _float() {
            return this._float;
        }
        public TestEndpointParametersRequest _float(@javax.annotation.Nullable Float _float) {
            this._float = _float;
            return this;
        }

        public @javax.annotation.Nullable String string() {
            return this.string;
        }
        public TestEndpointParametersRequest string(@javax.annotation.Nullable String string) {
            this.string = string;
            return this;
        }

        public @javax.annotation.Nullable File binary() {
            return this.binary;
        }
        public TestEndpointParametersRequest binary(@javax.annotation.Nullable File binary) {
            this.binary = binary;
            return this;
        }

        public @javax.annotation.Nullable LocalDate date() {
            return this.date;
        }
        public TestEndpointParametersRequest date(@javax.annotation.Nullable LocalDate date) {
            this.date = date;
            return this;
        }

        public @javax.annotation.Nullable OffsetDateTime dateTime() {
            return this.dateTime;
        }
        public TestEndpointParametersRequest dateTime(@javax.annotation.Nullable OffsetDateTime dateTime) {
            this.dateTime = dateTime;
            return this;
        }

        public @javax.annotation.Nullable String password() {
            return this.password;
        }
        public TestEndpointParametersRequest password(@javax.annotation.Nullable String password) {
            this.password = password;
            return this;
        }

        public @javax.annotation.Nullable String paramCallback() {
            return this.paramCallback;
        }
        public TestEndpointParametersRequest paramCallback(@javax.annotation.Nullable String paramCallback) {
            this.paramCallback = paramCallback;
            return this;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) {
                return true;
            }
            if (o == null || getClass() != o.getClass()) {
                return false;
            }
            TestEndpointParametersRequest request = (TestEndpointParametersRequest) o;
            return Objects.equals(this.number, request.number()) &&
                Objects.equals(this._double, request._double()) &&
                Objects.equals(this.patternWithoutDelimiter, request.patternWithoutDelimiter()) &&
                Arrays.equals(this._byte, request._byte()) &&
                Objects.equals(this.integer, request.integer()) &&
                Objects.equals(this.int32, request.int32()) &&
                Objects.equals(this.int64, request.int64()) &&
                Objects.equals(this._float, request._float()) &&
                Objects.equals(this.string, request.string()) &&
                Objects.equals(this.binary, request.binary()) &&
                Objects.equals(this.date, request.date()) &&
                Objects.equals(this.dateTime, request.dateTime()) &&
                Objects.equals(this.password, request.password()) &&
                Objects.equals(this.paramCallback, request.paramCallback());
        }

        @Override
        public int hashCode() {
            return Objects.hash(number, _double, patternWithoutDelimiter, Arrays.hashCode(_byte), integer, int32, int64, _float, string, binary, date, dateTime, password, paramCallback);
        }
    }

    /**
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     * <p><b>400</b> - Invalid username supplied
     * <p><b>404</b> - User not found
     * @param requestParameters The testEndpointParameters request parameters as object
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Void> testEndpointParameters(TestEndpointParametersRequest requestParameters) throws WebClientResponseException {
        return this.testEndpointParameters(requestParameters.number(), requestParameters._double(), requestParameters.patternWithoutDelimiter(), requestParameters._byte(), requestParameters.integer(), requestParameters.int32(), requestParameters.int64(), requestParameters._float(), requestParameters.string(), requestParameters.binary(), requestParameters.date(), requestParameters.dateTime(), requestParameters.password(), requestParameters.paramCallback());
    }

    /**
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     * <p><b>400</b> - Invalid username supplied
     * <p><b>404</b> - User not found
     * @param requestParameters The testEndpointParameters request parameters as object
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<Void>> testEndpointParametersWithHttpInfo(TestEndpointParametersRequest requestParameters) throws WebClientResponseException {
        return this.testEndpointParametersWithHttpInfo(requestParameters.number(), requestParameters._double(), requestParameters.patternWithoutDelimiter(), requestParameters._byte(), requestParameters.integer(), requestParameters.int32(), requestParameters.int64(), requestParameters._float(), requestParameters.string(), requestParameters.binary(), requestParameters.date(), requestParameters.dateTime(), requestParameters.password(), requestParameters.paramCallback());
    }

    /**
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     * <p><b>400</b> - Invalid username supplied
     * <p><b>404</b> - User not found
     * @param requestParameters The testEndpointParameters request parameters as object
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testEndpointParametersWithResponseSpec(TestEndpointParametersRequest requestParameters) throws WebClientResponseException {
       return this.testEndpointParametersWithResponseSpec(requestParameters.number(), requestParameters._double(), requestParameters.patternWithoutDelimiter(), requestParameters._byte(), requestParameters.integer(), requestParameters.int32(), requestParameters.int64(), requestParameters._float(), requestParameters.string(), requestParameters.binary(), requestParameters.date(), requestParameters.dateTime(), requestParameters.password(), requestParameters.paramCallback());
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
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testEndpointParametersRequestCreation(@javax.annotation.Nonnull BigDecimal number, @javax.annotation.Nonnull Double _double, @javax.annotation.Nonnull String patternWithoutDelimiter, @javax.annotation.Nonnull byte[] _byte, @javax.annotation.Nullable Integer integer, @javax.annotation.Nullable Integer int32, @javax.annotation.Nullable Long int64, @javax.annotation.Nullable Float _float, @javax.annotation.Nullable String string, @javax.annotation.Nullable File binary, @javax.annotation.Nullable LocalDate date, @javax.annotation.Nullable OffsetDateTime dateTime, @javax.annotation.Nullable String password, @javax.annotation.Nullable String paramCallback) throws WebClientResponseException {
        Object postBody = null;
        // verify the required parameter 'number' is set
        if (number == null) {
            throw new WebClientResponseException("Missing the required parameter 'number' when calling testEndpointParameters", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // verify the required parameter '_double' is set
        if (_double == null) {
            throw new WebClientResponseException("Missing the required parameter '_double' when calling testEndpointParameters", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // verify the required parameter 'patternWithoutDelimiter' is set
        if (patternWithoutDelimiter == null) {
            throw new WebClientResponseException("Missing the required parameter 'patternWithoutDelimiter' when calling testEndpointParameters", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // verify the required parameter '_byte' is set
        if (_byte == null) {
            throw new WebClientResponseException("Missing the required parameter '_byte' when calling testEndpointParameters", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

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

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
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
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Void> testEndpointParameters(@javax.annotation.Nonnull BigDecimal number, @javax.annotation.Nonnull Double _double, @javax.annotation.Nonnull String patternWithoutDelimiter, @javax.annotation.Nonnull byte[] _byte, @javax.annotation.Nullable Integer integer, @javax.annotation.Nullable Integer int32, @javax.annotation.Nullable Long int64, @javax.annotation.Nullable Float _float, @javax.annotation.Nullable String string, @javax.annotation.Nullable File binary, @javax.annotation.Nullable LocalDate date, @javax.annotation.Nullable OffsetDateTime dateTime, @javax.annotation.Nullable String password, @javax.annotation.Nullable String paramCallback) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return testEndpointParametersRequestCreation(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, paramCallback).bodyToMono(localVarReturnType);
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
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<Void>> testEndpointParametersWithHttpInfo(@javax.annotation.Nonnull BigDecimal number, @javax.annotation.Nonnull Double _double, @javax.annotation.Nonnull String patternWithoutDelimiter, @javax.annotation.Nonnull byte[] _byte, @javax.annotation.Nullable Integer integer, @javax.annotation.Nullable Integer int32, @javax.annotation.Nullable Long int64, @javax.annotation.Nullable Float _float, @javax.annotation.Nullable String string, @javax.annotation.Nullable File binary, @javax.annotation.Nullable LocalDate date, @javax.annotation.Nullable OffsetDateTime dateTime, @javax.annotation.Nullable String password, @javax.annotation.Nullable String paramCallback) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
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
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testEndpointParametersWithResponseSpec(@javax.annotation.Nonnull BigDecimal number, @javax.annotation.Nonnull Double _double, @javax.annotation.Nonnull String patternWithoutDelimiter, @javax.annotation.Nonnull byte[] _byte, @javax.annotation.Nullable Integer integer, @javax.annotation.Nullable Integer int32, @javax.annotation.Nullable Long int64, @javax.annotation.Nullable Float _float, @javax.annotation.Nullable String string, @javax.annotation.Nullable File binary, @javax.annotation.Nullable LocalDate date, @javax.annotation.Nullable OffsetDateTime dateTime, @javax.annotation.Nullable String password, @javax.annotation.Nullable String paramCallback) throws WebClientResponseException {
        return testEndpointParametersRequestCreation(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, paramCallback);
    }

    public class TestEnumParametersRequest {
        private @javax.annotation.Nullable List<String> enumHeaderStringArray;
        private @javax.annotation.Nullable String enumHeaderString;
        private @javax.annotation.Nullable List<String> enumQueryStringArray;
        private @javax.annotation.Nullable String enumQueryString;
        private @javax.annotation.Nullable Integer enumQueryInteger;
        private @javax.annotation.Nullable Double enumQueryDouble;
        private @javax.annotation.Nullable List<EnumClass> enumQueryModelArray;
        private @javax.annotation.Nullable List<String> enumFormStringArray;
        private @javax.annotation.Nullable String enumFormString;

        public TestEnumParametersRequest() {}

        public TestEnumParametersRequest(@javax.annotation.Nullable List<String> enumHeaderStringArray, @javax.annotation.Nullable String enumHeaderString, @javax.annotation.Nullable List<String> enumQueryStringArray, @javax.annotation.Nullable String enumQueryString, @javax.annotation.Nullable Integer enumQueryInteger, @javax.annotation.Nullable Double enumQueryDouble, @javax.annotation.Nullable List<EnumClass> enumQueryModelArray, @javax.annotation.Nullable List<String> enumFormStringArray, @javax.annotation.Nullable String enumFormString) {
            this.enumHeaderStringArray = enumHeaderStringArray;
            this.enumHeaderString = enumHeaderString;
            this.enumQueryStringArray = enumQueryStringArray;
            this.enumQueryString = enumQueryString;
            this.enumQueryInteger = enumQueryInteger;
            this.enumQueryDouble = enumQueryDouble;
            this.enumQueryModelArray = enumQueryModelArray;
            this.enumFormStringArray = enumFormStringArray;
            this.enumFormString = enumFormString;
        }

        public @javax.annotation.Nullable List<String> enumHeaderStringArray() {
            return this.enumHeaderStringArray;
        }
        public TestEnumParametersRequest enumHeaderStringArray(@javax.annotation.Nullable List<String> enumHeaderStringArray) {
            this.enumHeaderStringArray = enumHeaderStringArray;
            return this;
        }

        public @javax.annotation.Nullable String enumHeaderString() {
            return this.enumHeaderString;
        }
        public TestEnumParametersRequest enumHeaderString(@javax.annotation.Nullable String enumHeaderString) {
            this.enumHeaderString = enumHeaderString;
            return this;
        }

        public @javax.annotation.Nullable List<String> enumQueryStringArray() {
            return this.enumQueryStringArray;
        }
        public TestEnumParametersRequest enumQueryStringArray(@javax.annotation.Nullable List<String> enumQueryStringArray) {
            this.enumQueryStringArray = enumQueryStringArray;
            return this;
        }

        public @javax.annotation.Nullable String enumQueryString() {
            return this.enumQueryString;
        }
        public TestEnumParametersRequest enumQueryString(@javax.annotation.Nullable String enumQueryString) {
            this.enumQueryString = enumQueryString;
            return this;
        }

        public @javax.annotation.Nullable Integer enumQueryInteger() {
            return this.enumQueryInteger;
        }
        public TestEnumParametersRequest enumQueryInteger(@javax.annotation.Nullable Integer enumQueryInteger) {
            this.enumQueryInteger = enumQueryInteger;
            return this;
        }

        public @javax.annotation.Nullable Double enumQueryDouble() {
            return this.enumQueryDouble;
        }
        public TestEnumParametersRequest enumQueryDouble(@javax.annotation.Nullable Double enumQueryDouble) {
            this.enumQueryDouble = enumQueryDouble;
            return this;
        }

        public @javax.annotation.Nullable List<EnumClass> enumQueryModelArray() {
            return this.enumQueryModelArray;
        }
        public TestEnumParametersRequest enumQueryModelArray(@javax.annotation.Nullable List<EnumClass> enumQueryModelArray) {
            this.enumQueryModelArray = enumQueryModelArray;
            return this;
        }

        public @javax.annotation.Nullable List<String> enumFormStringArray() {
            return this.enumFormStringArray;
        }
        public TestEnumParametersRequest enumFormStringArray(@javax.annotation.Nullable List<String> enumFormStringArray) {
            this.enumFormStringArray = enumFormStringArray;
            return this;
        }

        public @javax.annotation.Nullable String enumFormString() {
            return this.enumFormString;
        }
        public TestEnumParametersRequest enumFormString(@javax.annotation.Nullable String enumFormString) {
            this.enumFormString = enumFormString;
            return this;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) {
                return true;
            }
            if (o == null || getClass() != o.getClass()) {
                return false;
            }
            TestEnumParametersRequest request = (TestEnumParametersRequest) o;
            return Objects.equals(this.enumHeaderStringArray, request.enumHeaderStringArray()) &&
                Objects.equals(this.enumHeaderString, request.enumHeaderString()) &&
                Objects.equals(this.enumQueryStringArray, request.enumQueryStringArray()) &&
                Objects.equals(this.enumQueryString, request.enumQueryString()) &&
                Objects.equals(this.enumQueryInteger, request.enumQueryInteger()) &&
                Objects.equals(this.enumQueryDouble, request.enumQueryDouble()) &&
                Objects.equals(this.enumQueryModelArray, request.enumQueryModelArray()) &&
                Objects.equals(this.enumFormStringArray, request.enumFormStringArray()) &&
                Objects.equals(this.enumFormString, request.enumFormString());
        }

        @Override
        public int hashCode() {
            return Objects.hash(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumQueryModelArray, enumFormStringArray, enumFormString);
        }
    }

    /**
     * To test enum parameters
     * To test enum parameters
     * <p><b>400</b> - Invalid request
     * <p><b>404</b> - Not found
     * @param requestParameters The testEnumParameters request parameters as object
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Void> testEnumParameters(TestEnumParametersRequest requestParameters) throws WebClientResponseException {
        return this.testEnumParameters(requestParameters.enumHeaderStringArray(), requestParameters.enumHeaderString(), requestParameters.enumQueryStringArray(), requestParameters.enumQueryString(), requestParameters.enumQueryInteger(), requestParameters.enumQueryDouble(), requestParameters.enumQueryModelArray(), requestParameters.enumFormStringArray(), requestParameters.enumFormString());
    }

    /**
     * To test enum parameters
     * To test enum parameters
     * <p><b>400</b> - Invalid request
     * <p><b>404</b> - Not found
     * @param requestParameters The testEnumParameters request parameters as object
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<Void>> testEnumParametersWithHttpInfo(TestEnumParametersRequest requestParameters) throws WebClientResponseException {
        return this.testEnumParametersWithHttpInfo(requestParameters.enumHeaderStringArray(), requestParameters.enumHeaderString(), requestParameters.enumQueryStringArray(), requestParameters.enumQueryString(), requestParameters.enumQueryInteger(), requestParameters.enumQueryDouble(), requestParameters.enumQueryModelArray(), requestParameters.enumFormStringArray(), requestParameters.enumFormString());
    }

    /**
     * To test enum parameters
     * To test enum parameters
     * <p><b>400</b> - Invalid request
     * <p><b>404</b> - Not found
     * @param requestParameters The testEnumParameters request parameters as object
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testEnumParametersWithResponseSpec(TestEnumParametersRequest requestParameters) throws WebClientResponseException {
       return this.testEnumParametersWithResponseSpec(requestParameters.enumHeaderStringArray(), requestParameters.enumHeaderString(), requestParameters.enumQueryStringArray(), requestParameters.enumQueryString(), requestParameters.enumQueryInteger(), requestParameters.enumQueryDouble(), requestParameters.enumQueryModelArray(), requestParameters.enumFormStringArray(), requestParameters.enumFormString());
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
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testEnumParametersRequestCreation(@javax.annotation.Nullable List<String> enumHeaderStringArray, @javax.annotation.Nullable String enumHeaderString, @javax.annotation.Nullable List<String> enumQueryStringArray, @javax.annotation.Nullable String enumQueryString, @javax.annotation.Nullable Integer enumQueryInteger, @javax.annotation.Nullable Double enumQueryDouble, @javax.annotation.Nullable List<EnumClass> enumQueryModelArray, @javax.annotation.Nullable List<String> enumFormStringArray, @javax.annotation.Nullable String enumFormString) throws WebClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

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

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
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
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Void> testEnumParameters(@javax.annotation.Nullable List<String> enumHeaderStringArray, @javax.annotation.Nullable String enumHeaderString, @javax.annotation.Nullable List<String> enumQueryStringArray, @javax.annotation.Nullable String enumQueryString, @javax.annotation.Nullable Integer enumQueryInteger, @javax.annotation.Nullable Double enumQueryDouble, @javax.annotation.Nullable List<EnumClass> enumQueryModelArray, @javax.annotation.Nullable List<String> enumFormStringArray, @javax.annotation.Nullable String enumFormString) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return testEnumParametersRequestCreation(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumQueryModelArray, enumFormStringArray, enumFormString).bodyToMono(localVarReturnType);
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
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<Void>> testEnumParametersWithHttpInfo(@javax.annotation.Nullable List<String> enumHeaderStringArray, @javax.annotation.Nullable String enumHeaderString, @javax.annotation.Nullable List<String> enumQueryStringArray, @javax.annotation.Nullable String enumQueryString, @javax.annotation.Nullable Integer enumQueryInteger, @javax.annotation.Nullable Double enumQueryDouble, @javax.annotation.Nullable List<EnumClass> enumQueryModelArray, @javax.annotation.Nullable List<String> enumFormStringArray, @javax.annotation.Nullable String enumFormString) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
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
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testEnumParametersWithResponseSpec(@javax.annotation.Nullable List<String> enumHeaderStringArray, @javax.annotation.Nullable String enumHeaderString, @javax.annotation.Nullable List<String> enumQueryStringArray, @javax.annotation.Nullable String enumQueryString, @javax.annotation.Nullable Integer enumQueryInteger, @javax.annotation.Nullable Double enumQueryDouble, @javax.annotation.Nullable List<EnumClass> enumQueryModelArray, @javax.annotation.Nullable List<String> enumFormStringArray, @javax.annotation.Nullable String enumFormString) throws WebClientResponseException {
        return testEnumParametersRequestCreation(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumQueryModelArray, enumFormStringArray, enumFormString);
    }

    public class TestGroupParametersRequest {
        private @javax.annotation.Nonnull Integer requiredStringGroup;
        private @javax.annotation.Nonnull Boolean requiredBooleanGroup;
        private @javax.annotation.Nonnull Long requiredInt64Group;
        private @javax.annotation.Nullable Integer stringGroup;
        private @javax.annotation.Nullable Boolean booleanGroup;
        private @javax.annotation.Nullable Long int64Group;

        public TestGroupParametersRequest() {}

        public TestGroupParametersRequest(@javax.annotation.Nonnull Integer requiredStringGroup, @javax.annotation.Nonnull Boolean requiredBooleanGroup, @javax.annotation.Nonnull Long requiredInt64Group, @javax.annotation.Nullable Integer stringGroup, @javax.annotation.Nullable Boolean booleanGroup, @javax.annotation.Nullable Long int64Group) {
            this.requiredStringGroup = requiredStringGroup;
            this.requiredBooleanGroup = requiredBooleanGroup;
            this.requiredInt64Group = requiredInt64Group;
            this.stringGroup = stringGroup;
            this.booleanGroup = booleanGroup;
            this.int64Group = int64Group;
        }

        public @javax.annotation.Nonnull Integer requiredStringGroup() {
            return this.requiredStringGroup;
        }
        public TestGroupParametersRequest requiredStringGroup(@javax.annotation.Nonnull Integer requiredStringGroup) {
            this.requiredStringGroup = requiredStringGroup;
            return this;
        }

        public @javax.annotation.Nonnull Boolean requiredBooleanGroup() {
            return this.requiredBooleanGroup;
        }
        public TestGroupParametersRequest requiredBooleanGroup(@javax.annotation.Nonnull Boolean requiredBooleanGroup) {
            this.requiredBooleanGroup = requiredBooleanGroup;
            return this;
        }

        public @javax.annotation.Nonnull Long requiredInt64Group() {
            return this.requiredInt64Group;
        }
        public TestGroupParametersRequest requiredInt64Group(@javax.annotation.Nonnull Long requiredInt64Group) {
            this.requiredInt64Group = requiredInt64Group;
            return this;
        }

        public @javax.annotation.Nullable Integer stringGroup() {
            return this.stringGroup;
        }
        public TestGroupParametersRequest stringGroup(@javax.annotation.Nullable Integer stringGroup) {
            this.stringGroup = stringGroup;
            return this;
        }

        public @javax.annotation.Nullable Boolean booleanGroup() {
            return this.booleanGroup;
        }
        public TestGroupParametersRequest booleanGroup(@javax.annotation.Nullable Boolean booleanGroup) {
            this.booleanGroup = booleanGroup;
            return this;
        }

        public @javax.annotation.Nullable Long int64Group() {
            return this.int64Group;
        }
        public TestGroupParametersRequest int64Group(@javax.annotation.Nullable Long int64Group) {
            this.int64Group = int64Group;
            return this;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) {
                return true;
            }
            if (o == null || getClass() != o.getClass()) {
                return false;
            }
            TestGroupParametersRequest request = (TestGroupParametersRequest) o;
            return Objects.equals(this.requiredStringGroup, request.requiredStringGroup()) &&
                Objects.equals(this.requiredBooleanGroup, request.requiredBooleanGroup()) &&
                Objects.equals(this.requiredInt64Group, request.requiredInt64Group()) &&
                Objects.equals(this.stringGroup, request.stringGroup()) &&
                Objects.equals(this.booleanGroup, request.booleanGroup()) &&
                Objects.equals(this.int64Group, request.int64Group());
        }

        @Override
        public int hashCode() {
            return Objects.hash(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, stringGroup, booleanGroup, int64Group);
        }
    }

    /**
     * Fake endpoint to test group parameters (optional)
     * Fake endpoint to test group parameters (optional)
     * <p><b>400</b> - Something wrong
     * @param requestParameters The testGroupParameters request parameters as object
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Void> testGroupParameters(TestGroupParametersRequest requestParameters) throws WebClientResponseException {
        return this.testGroupParameters(requestParameters.requiredStringGroup(), requestParameters.requiredBooleanGroup(), requestParameters.requiredInt64Group(), requestParameters.stringGroup(), requestParameters.booleanGroup(), requestParameters.int64Group());
    }

    /**
     * Fake endpoint to test group parameters (optional)
     * Fake endpoint to test group parameters (optional)
     * <p><b>400</b> - Something wrong
     * @param requestParameters The testGroupParameters request parameters as object
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<Void>> testGroupParametersWithHttpInfo(TestGroupParametersRequest requestParameters) throws WebClientResponseException {
        return this.testGroupParametersWithHttpInfo(requestParameters.requiredStringGroup(), requestParameters.requiredBooleanGroup(), requestParameters.requiredInt64Group(), requestParameters.stringGroup(), requestParameters.booleanGroup(), requestParameters.int64Group());
    }

    /**
     * Fake endpoint to test group parameters (optional)
     * Fake endpoint to test group parameters (optional)
     * <p><b>400</b> - Something wrong
     * @param requestParameters The testGroupParameters request parameters as object
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testGroupParametersWithResponseSpec(TestGroupParametersRequest requestParameters) throws WebClientResponseException {
       return this.testGroupParametersWithResponseSpec(requestParameters.requiredStringGroup(), requestParameters.requiredBooleanGroup(), requestParameters.requiredInt64Group(), requestParameters.stringGroup(), requestParameters.booleanGroup(), requestParameters.int64Group());
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
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testGroupParametersRequestCreation(@javax.annotation.Nonnull Integer requiredStringGroup, @javax.annotation.Nonnull Boolean requiredBooleanGroup, @javax.annotation.Nonnull Long requiredInt64Group, @javax.annotation.Nullable Integer stringGroup, @javax.annotation.Nullable Boolean booleanGroup, @javax.annotation.Nullable Long int64Group) throws WebClientResponseException {
        Object postBody = null;
        // verify the required parameter 'requiredStringGroup' is set
        if (requiredStringGroup == null) {
            throw new WebClientResponseException("Missing the required parameter 'requiredStringGroup' when calling testGroupParameters", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // verify the required parameter 'requiredBooleanGroup' is set
        if (requiredBooleanGroup == null) {
            throw new WebClientResponseException("Missing the required parameter 'requiredBooleanGroup' when calling testGroupParameters", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // verify the required parameter 'requiredInt64Group' is set
        if (requiredInt64Group == null) {
            throw new WebClientResponseException("Missing the required parameter 'requiredInt64Group' when calling testGroupParameters", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

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

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
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
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Void> testGroupParameters(@javax.annotation.Nonnull Integer requiredStringGroup, @javax.annotation.Nonnull Boolean requiredBooleanGroup, @javax.annotation.Nonnull Long requiredInt64Group, @javax.annotation.Nullable Integer stringGroup, @javax.annotation.Nullable Boolean booleanGroup, @javax.annotation.Nullable Long int64Group) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return testGroupParametersRequestCreation(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, stringGroup, booleanGroup, int64Group).bodyToMono(localVarReturnType);
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
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<Void>> testGroupParametersWithHttpInfo(@javax.annotation.Nonnull Integer requiredStringGroup, @javax.annotation.Nonnull Boolean requiredBooleanGroup, @javax.annotation.Nonnull Long requiredInt64Group, @javax.annotation.Nullable Integer stringGroup, @javax.annotation.Nullable Boolean booleanGroup, @javax.annotation.Nullable Long int64Group) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
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
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testGroupParametersWithResponseSpec(@javax.annotation.Nonnull Integer requiredStringGroup, @javax.annotation.Nonnull Boolean requiredBooleanGroup, @javax.annotation.Nonnull Long requiredInt64Group, @javax.annotation.Nullable Integer stringGroup, @javax.annotation.Nullable Boolean booleanGroup, @javax.annotation.Nullable Long int64Group) throws WebClientResponseException {
        return testGroupParametersRequestCreation(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, stringGroup, booleanGroup, int64Group);
    }

    /**
     * test inline additionalProperties
     * 
     * <p><b>200</b> - successful operation
     * @param requestBody request body
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testInlineAdditionalPropertiesRequestCreation(@javax.annotation.Nonnull Map<String, String> requestBody) throws WebClientResponseException {
        Object postBody = requestBody;
        // verify the required parameter 'requestBody' is set
        if (requestBody == null) {
            throw new WebClientResponseException("Missing the required parameter 'requestBody' when calling testInlineAdditionalProperties", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return apiClient.invokeAPI("/fake/inline-additionalProperties", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * test inline additionalProperties
     * 
     * <p><b>200</b> - successful operation
     * @param requestBody request body
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Void> testInlineAdditionalProperties(@javax.annotation.Nonnull Map<String, String> requestBody) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return testInlineAdditionalPropertiesRequestCreation(requestBody).bodyToMono(localVarReturnType);
    }

    /**
     * test inline additionalProperties
     * 
     * <p><b>200</b> - successful operation
     * @param requestBody request body
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<Void>> testInlineAdditionalPropertiesWithHttpInfo(@javax.annotation.Nonnull Map<String, String> requestBody) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return testInlineAdditionalPropertiesRequestCreation(requestBody).toEntity(localVarReturnType);
    }

    /**
     * test inline additionalProperties
     * 
     * <p><b>200</b> - successful operation
     * @param requestBody request body
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testInlineAdditionalPropertiesWithResponseSpec(@javax.annotation.Nonnull Map<String, String> requestBody) throws WebClientResponseException {
        return testInlineAdditionalPropertiesRequestCreation(requestBody);
    }

    /**
     * test inline free-form additionalProperties
     * 
     * <p><b>200</b> - successful operation
     * @param testInlineFreeformAdditionalPropertiesRequest request body
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testInlineFreeformAdditionalPropertiesRequestCreation(@javax.annotation.Nonnull TestInlineFreeformAdditionalPropertiesRequest testInlineFreeformAdditionalPropertiesRequest) throws WebClientResponseException {
        Object postBody = testInlineFreeformAdditionalPropertiesRequest;
        // verify the required parameter 'testInlineFreeformAdditionalPropertiesRequest' is set
        if (testInlineFreeformAdditionalPropertiesRequest == null) {
            throw new WebClientResponseException("Missing the required parameter 'testInlineFreeformAdditionalPropertiesRequest' when calling testInlineFreeformAdditionalProperties", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return apiClient.invokeAPI("/fake/inline-freeform-additionalProperties", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * test inline free-form additionalProperties
     * 
     * <p><b>200</b> - successful operation
     * @param testInlineFreeformAdditionalPropertiesRequest request body
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Void> testInlineFreeformAdditionalProperties(@javax.annotation.Nonnull TestInlineFreeformAdditionalPropertiesRequest testInlineFreeformAdditionalPropertiesRequest) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return testInlineFreeformAdditionalPropertiesRequestCreation(testInlineFreeformAdditionalPropertiesRequest).bodyToMono(localVarReturnType);
    }

    /**
     * test inline free-form additionalProperties
     * 
     * <p><b>200</b> - successful operation
     * @param testInlineFreeformAdditionalPropertiesRequest request body
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<Void>> testInlineFreeformAdditionalPropertiesWithHttpInfo(@javax.annotation.Nonnull TestInlineFreeformAdditionalPropertiesRequest testInlineFreeformAdditionalPropertiesRequest) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return testInlineFreeformAdditionalPropertiesRequestCreation(testInlineFreeformAdditionalPropertiesRequest).toEntity(localVarReturnType);
    }

    /**
     * test inline free-form additionalProperties
     * 
     * <p><b>200</b> - successful operation
     * @param testInlineFreeformAdditionalPropertiesRequest request body
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testInlineFreeformAdditionalPropertiesWithResponseSpec(@javax.annotation.Nonnull TestInlineFreeformAdditionalPropertiesRequest testInlineFreeformAdditionalPropertiesRequest) throws WebClientResponseException {
        return testInlineFreeformAdditionalPropertiesRequestCreation(testInlineFreeformAdditionalPropertiesRequest);
    }

    public class TestJsonFormDataRequest {
        private @javax.annotation.Nonnull String param;
        private @javax.annotation.Nonnull String param2;

        public TestJsonFormDataRequest() {}

        public TestJsonFormDataRequest(@javax.annotation.Nonnull String param, @javax.annotation.Nonnull String param2) {
            this.param = param;
            this.param2 = param2;
        }

        public @javax.annotation.Nonnull String param() {
            return this.param;
        }
        public TestJsonFormDataRequest param(@javax.annotation.Nonnull String param) {
            this.param = param;
            return this;
        }

        public @javax.annotation.Nonnull String param2() {
            return this.param2;
        }
        public TestJsonFormDataRequest param2(@javax.annotation.Nonnull String param2) {
            this.param2 = param2;
            return this;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) {
                return true;
            }
            if (o == null || getClass() != o.getClass()) {
                return false;
            }
            TestJsonFormDataRequest request = (TestJsonFormDataRequest) o;
            return Objects.equals(this.param, request.param()) &&
                Objects.equals(this.param2, request.param2());
        }

        @Override
        public int hashCode() {
            return Objects.hash(param, param2);
        }
    }

    /**
     * test json serialization of form data
     * 
     * <p><b>200</b> - successful operation
     * @param requestParameters The testJsonFormData request parameters as object
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Void> testJsonFormData(TestJsonFormDataRequest requestParameters) throws WebClientResponseException {
        return this.testJsonFormData(requestParameters.param(), requestParameters.param2());
    }

    /**
     * test json serialization of form data
     * 
     * <p><b>200</b> - successful operation
     * @param requestParameters The testJsonFormData request parameters as object
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<Void>> testJsonFormDataWithHttpInfo(TestJsonFormDataRequest requestParameters) throws WebClientResponseException {
        return this.testJsonFormDataWithHttpInfo(requestParameters.param(), requestParameters.param2());
    }

    /**
     * test json serialization of form data
     * 
     * <p><b>200</b> - successful operation
     * @param requestParameters The testJsonFormData request parameters as object
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testJsonFormDataWithResponseSpec(TestJsonFormDataRequest requestParameters) throws WebClientResponseException {
       return this.testJsonFormDataWithResponseSpec(requestParameters.param(), requestParameters.param2());
    }

          
    /**
     * test json serialization of form data
     * 
     * <p><b>200</b> - successful operation
     * @param param field1
     * @param param2 field2
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testJsonFormDataRequestCreation(@javax.annotation.Nonnull String param, @javax.annotation.Nonnull String param2) throws WebClientResponseException {
        Object postBody = null;
        // verify the required parameter 'param' is set
        if (param == null) {
            throw new WebClientResponseException("Missing the required parameter 'param' when calling testJsonFormData", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // verify the required parameter 'param2' is set
        if (param2 == null) {
            throw new WebClientResponseException("Missing the required parameter 'param2' when calling testJsonFormData", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

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

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return apiClient.invokeAPI("/fake/jsonFormData", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * test json serialization of form data
     * 
     * <p><b>200</b> - successful operation
     * @param param field1
     * @param param2 field2
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Void> testJsonFormData(@javax.annotation.Nonnull String param, @javax.annotation.Nonnull String param2) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return testJsonFormDataRequestCreation(param, param2).bodyToMono(localVarReturnType);
    }

    /**
     * test json serialization of form data
     * 
     * <p><b>200</b> - successful operation
     * @param param field1
     * @param param2 field2
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<Void>> testJsonFormDataWithHttpInfo(@javax.annotation.Nonnull String param, @javax.annotation.Nonnull String param2) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return testJsonFormDataRequestCreation(param, param2).toEntity(localVarReturnType);
    }

    /**
     * test json serialization of form data
     * 
     * <p><b>200</b> - successful operation
     * @param param field1
     * @param param2 field2
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testJsonFormDataWithResponseSpec(@javax.annotation.Nonnull String param, @javax.annotation.Nonnull String param2) throws WebClientResponseException {
        return testJsonFormDataRequestCreation(param, param2);
    }

    /**
     * test nullable parent property
     * 
     * <p><b>200</b> - successful operation
     * @param childWithNullable request body
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testNullableRequestCreation(@javax.annotation.Nonnull ChildWithNullable childWithNullable) throws WebClientResponseException {
        Object postBody = childWithNullable;
        // verify the required parameter 'childWithNullable' is set
        if (childWithNullable == null) {
            throw new WebClientResponseException("Missing the required parameter 'childWithNullable' when calling testNullable", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return apiClient.invokeAPI("/fake/nullable", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * test nullable parent property
     * 
     * <p><b>200</b> - successful operation
     * @param childWithNullable request body
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Void> testNullable(@javax.annotation.Nonnull ChildWithNullable childWithNullable) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return testNullableRequestCreation(childWithNullable).bodyToMono(localVarReturnType);
    }

    /**
     * test nullable parent property
     * 
     * <p><b>200</b> - successful operation
     * @param childWithNullable request body
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<Void>> testNullableWithHttpInfo(@javax.annotation.Nonnull ChildWithNullable childWithNullable) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return testNullableRequestCreation(childWithNullable).toEntity(localVarReturnType);
    }

    /**
     * test nullable parent property
     * 
     * <p><b>200</b> - successful operation
     * @param childWithNullable request body
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testNullableWithResponseSpec(@javax.annotation.Nonnull ChildWithNullable childWithNullable) throws WebClientResponseException {
        return testNullableRequestCreation(childWithNullable);
    }

    public class TestQueryParameterCollectionFormatRequest {
        private @javax.annotation.Nonnull List<String> pipe;
        private @javax.annotation.Nonnull List<String> ioutil;
        private @javax.annotation.Nonnull List<String> http;
        private @javax.annotation.Nonnull List<String> url;
        private @javax.annotation.Nonnull List<String> context;
        private @javax.annotation.Nonnull String allowEmpty;
        private @javax.annotation.Nullable Map<String, String> language;

        public TestQueryParameterCollectionFormatRequest() {}

        public TestQueryParameterCollectionFormatRequest(@javax.annotation.Nonnull List<String> pipe, @javax.annotation.Nonnull List<String> ioutil, @javax.annotation.Nonnull List<String> http, @javax.annotation.Nonnull List<String> url, @javax.annotation.Nonnull List<String> context, @javax.annotation.Nonnull String allowEmpty, @javax.annotation.Nullable Map<String, String> language) {
            this.pipe = pipe;
            this.ioutil = ioutil;
            this.http = http;
            this.url = url;
            this.context = context;
            this.allowEmpty = allowEmpty;
            this.language = language;
        }

        public @javax.annotation.Nonnull List<String> pipe() {
            return this.pipe;
        }
        public TestQueryParameterCollectionFormatRequest pipe(@javax.annotation.Nonnull List<String> pipe) {
            this.pipe = pipe;
            return this;
        }

        public @javax.annotation.Nonnull List<String> ioutil() {
            return this.ioutil;
        }
        public TestQueryParameterCollectionFormatRequest ioutil(@javax.annotation.Nonnull List<String> ioutil) {
            this.ioutil = ioutil;
            return this;
        }

        public @javax.annotation.Nonnull List<String> http() {
            return this.http;
        }
        public TestQueryParameterCollectionFormatRequest http(@javax.annotation.Nonnull List<String> http) {
            this.http = http;
            return this;
        }

        public @javax.annotation.Nonnull List<String> url() {
            return this.url;
        }
        public TestQueryParameterCollectionFormatRequest url(@javax.annotation.Nonnull List<String> url) {
            this.url = url;
            return this;
        }

        public @javax.annotation.Nonnull List<String> context() {
            return this.context;
        }
        public TestQueryParameterCollectionFormatRequest context(@javax.annotation.Nonnull List<String> context) {
            this.context = context;
            return this;
        }

        public @javax.annotation.Nonnull String allowEmpty() {
            return this.allowEmpty;
        }
        public TestQueryParameterCollectionFormatRequest allowEmpty(@javax.annotation.Nonnull String allowEmpty) {
            this.allowEmpty = allowEmpty;
            return this;
        }

        public @javax.annotation.Nullable Map<String, String> language() {
            return this.language;
        }
        public TestQueryParameterCollectionFormatRequest language(@javax.annotation.Nullable Map<String, String> language) {
            this.language = language;
            return this;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) {
                return true;
            }
            if (o == null || getClass() != o.getClass()) {
                return false;
            }
            TestQueryParameterCollectionFormatRequest request = (TestQueryParameterCollectionFormatRequest) o;
            return Objects.equals(this.pipe, request.pipe()) &&
                Objects.equals(this.ioutil, request.ioutil()) &&
                Objects.equals(this.http, request.http()) &&
                Objects.equals(this.url, request.url()) &&
                Objects.equals(this.context, request.context()) &&
                Objects.equals(this.allowEmpty, request.allowEmpty()) &&
                Objects.equals(this.language, request.language());
        }

        @Override
        public int hashCode() {
            return Objects.hash(pipe, ioutil, http, url, context, allowEmpty, language);
        }
    }

    /**
     * 
     * To test the collection format in query parameters
     * <p><b>200</b> - Success
     * @param requestParameters The testQueryParameterCollectionFormat request parameters as object
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Void> testQueryParameterCollectionFormat(TestQueryParameterCollectionFormatRequest requestParameters) throws WebClientResponseException {
        return this.testQueryParameterCollectionFormat(requestParameters.pipe(), requestParameters.ioutil(), requestParameters.http(), requestParameters.url(), requestParameters.context(), requestParameters.allowEmpty(), requestParameters.language());
    }

    /**
     * 
     * To test the collection format in query parameters
     * <p><b>200</b> - Success
     * @param requestParameters The testQueryParameterCollectionFormat request parameters as object
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<Void>> testQueryParameterCollectionFormatWithHttpInfo(TestQueryParameterCollectionFormatRequest requestParameters) throws WebClientResponseException {
        return this.testQueryParameterCollectionFormatWithHttpInfo(requestParameters.pipe(), requestParameters.ioutil(), requestParameters.http(), requestParameters.url(), requestParameters.context(), requestParameters.allowEmpty(), requestParameters.language());
    }

    /**
     * 
     * To test the collection format in query parameters
     * <p><b>200</b> - Success
     * @param requestParameters The testQueryParameterCollectionFormat request parameters as object
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testQueryParameterCollectionFormatWithResponseSpec(TestQueryParameterCollectionFormatRequest requestParameters) throws WebClientResponseException {
       return this.testQueryParameterCollectionFormatWithResponseSpec(requestParameters.pipe(), requestParameters.ioutil(), requestParameters.http(), requestParameters.url(), requestParameters.context(), requestParameters.allowEmpty(), requestParameters.language());
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
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testQueryParameterCollectionFormatRequestCreation(@javax.annotation.Nonnull List<String> pipe, @javax.annotation.Nonnull List<String> ioutil, @javax.annotation.Nonnull List<String> http, @javax.annotation.Nonnull List<String> url, @javax.annotation.Nonnull List<String> context, @javax.annotation.Nonnull String allowEmpty, @javax.annotation.Nullable Map<String, String> language) throws WebClientResponseException {
        Object postBody = null;
        // verify the required parameter 'pipe' is set
        if (pipe == null) {
            throw new WebClientResponseException("Missing the required parameter 'pipe' when calling testQueryParameterCollectionFormat", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // verify the required parameter 'ioutil' is set
        if (ioutil == null) {
            throw new WebClientResponseException("Missing the required parameter 'ioutil' when calling testQueryParameterCollectionFormat", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // verify the required parameter 'http' is set
        if (http == null) {
            throw new WebClientResponseException("Missing the required parameter 'http' when calling testQueryParameterCollectionFormat", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // verify the required parameter 'url' is set
        if (url == null) {
            throw new WebClientResponseException("Missing the required parameter 'url' when calling testQueryParameterCollectionFormat", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // verify the required parameter 'context' is set
        if (context == null) {
            throw new WebClientResponseException("Missing the required parameter 'context' when calling testQueryParameterCollectionFormat", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // verify the required parameter 'allowEmpty' is set
        if (allowEmpty == null) {
            throw new WebClientResponseException("Missing the required parameter 'allowEmpty' when calling testQueryParameterCollectionFormat", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

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

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
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
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Void> testQueryParameterCollectionFormat(@javax.annotation.Nonnull List<String> pipe, @javax.annotation.Nonnull List<String> ioutil, @javax.annotation.Nonnull List<String> http, @javax.annotation.Nonnull List<String> url, @javax.annotation.Nonnull List<String> context, @javax.annotation.Nonnull String allowEmpty, @javax.annotation.Nullable Map<String, String> language) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return testQueryParameterCollectionFormatRequestCreation(pipe, ioutil, http, url, context, allowEmpty, language).bodyToMono(localVarReturnType);
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
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<Void>> testQueryParameterCollectionFormatWithHttpInfo(@javax.annotation.Nonnull List<String> pipe, @javax.annotation.Nonnull List<String> ioutil, @javax.annotation.Nonnull List<String> http, @javax.annotation.Nonnull List<String> url, @javax.annotation.Nonnull List<String> context, @javax.annotation.Nonnull String allowEmpty, @javax.annotation.Nullable Map<String, String> language) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
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
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testQueryParameterCollectionFormatWithResponseSpec(@javax.annotation.Nonnull List<String> pipe, @javax.annotation.Nonnull List<String> ioutil, @javax.annotation.Nonnull List<String> http, @javax.annotation.Nonnull List<String> url, @javax.annotation.Nonnull List<String> context, @javax.annotation.Nonnull String allowEmpty, @javax.annotation.Nullable Map<String, String> language) throws WebClientResponseException {
        return testQueryParameterCollectionFormatRequestCreation(pipe, ioutil, http, url, context, allowEmpty, language);
    }

    /**
     * test referenced string map
     * 
     * <p><b>200</b> - successful operation
     * @param requestBody request body
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testStringMapReferenceRequestCreation(@javax.annotation.Nonnull Map<String, String> requestBody) throws WebClientResponseException {
        Object postBody = requestBody;
        // verify the required parameter 'requestBody' is set
        if (requestBody == null) {
            throw new WebClientResponseException("Missing the required parameter 'requestBody' when calling testStringMapReference", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<String, Object>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return apiClient.invokeAPI("/fake/stringMap-reference", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * test referenced string map
     * 
     * <p><b>200</b> - successful operation
     * @param requestBody request body
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<Void> testStringMapReference(@javax.annotation.Nonnull Map<String, String> requestBody) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return testStringMapReferenceRequestCreation(requestBody).bodyToMono(localVarReturnType);
    }

    /**
     * test referenced string map
     * 
     * <p><b>200</b> - successful operation
     * @param requestBody request body
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public Mono<ResponseEntity<Void>> testStringMapReferenceWithHttpInfo(@javax.annotation.Nonnull Map<String, String> requestBody) throws WebClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<Void>() {};
        return testStringMapReferenceRequestCreation(requestBody).toEntity(localVarReturnType);
    }

    /**
     * test referenced string map
     * 
     * <p><b>200</b> - successful operation
     * @param requestBody request body
     * @return ResponseSpec
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testStringMapReferenceWithResponseSpec(@javax.annotation.Nonnull Map<String, String> requestBody) throws WebClientResponseException {
        return testStringMapReferenceRequestCreation(requestBody);
    }
}
