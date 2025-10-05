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

@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
public class FakeApi {
    private ApiClient apiClient;

    public FakeApi() {
        this(new ApiClient());
    }

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

    /**
     * test http signature authentication
     * 
     * <p><b>200</b> - The instance started successfully
     * @param pet Pet object that needs to be added to the store
     * @param query1 query parameter
     * @param header1 header parameter
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec fakeHttpSignatureTestRequestCreation(@jakarta.annotation.Nonnull Pet pet, @jakarta.annotation.Nullable String query1, @jakarta.annotation.Nullable String header1) throws WebClientResponseException {
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
    public Mono<Void> fakeHttpSignatureTest(@jakarta.annotation.Nonnull Pet pet, @jakarta.annotation.Nullable String query1, @jakarta.annotation.Nullable String header1) throws WebClientResponseException {
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
    public Mono<ResponseEntity<Void>> fakeHttpSignatureTestWithHttpInfo(@jakarta.annotation.Nonnull Pet pet, @jakarta.annotation.Nullable String query1, @jakarta.annotation.Nullable String header1) throws WebClientResponseException {
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
    public ResponseSpec fakeHttpSignatureTestWithResponseSpec(@jakarta.annotation.Nonnull Pet pet, @jakarta.annotation.Nullable String query1, @jakarta.annotation.Nullable String header1) throws WebClientResponseException {
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
    private ResponseSpec fakeOuterBooleanSerializeRequestCreation(@jakarta.annotation.Nullable Boolean body) throws WebClientResponseException {
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
    public Mono<Boolean> fakeOuterBooleanSerialize(@jakarta.annotation.Nullable Boolean body) throws WebClientResponseException {
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
    public Mono<ResponseEntity<Boolean>> fakeOuterBooleanSerializeWithHttpInfo(@jakarta.annotation.Nullable Boolean body) throws WebClientResponseException {
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
    public ResponseSpec fakeOuterBooleanSerializeWithResponseSpec(@jakarta.annotation.Nullable Boolean body) throws WebClientResponseException {
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
    private ResponseSpec fakeOuterCompositeSerializeRequestCreation(@jakarta.annotation.Nullable OuterComposite outerComposite) throws WebClientResponseException {
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
    public Mono<OuterComposite> fakeOuterCompositeSerialize(@jakarta.annotation.Nullable OuterComposite outerComposite) throws WebClientResponseException {
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
    public Mono<ResponseEntity<OuterComposite>> fakeOuterCompositeSerializeWithHttpInfo(@jakarta.annotation.Nullable OuterComposite outerComposite) throws WebClientResponseException {
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
    public ResponseSpec fakeOuterCompositeSerializeWithResponseSpec(@jakarta.annotation.Nullable OuterComposite outerComposite) throws WebClientResponseException {
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
    private ResponseSpec fakeOuterNumberSerializeRequestCreation(@jakarta.annotation.Nullable BigDecimal body) throws WebClientResponseException {
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
    public Mono<BigDecimal> fakeOuterNumberSerialize(@jakarta.annotation.Nullable BigDecimal body) throws WebClientResponseException {
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
    public Mono<ResponseEntity<BigDecimal>> fakeOuterNumberSerializeWithHttpInfo(@jakarta.annotation.Nullable BigDecimal body) throws WebClientResponseException {
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
    public ResponseSpec fakeOuterNumberSerializeWithResponseSpec(@jakarta.annotation.Nullable BigDecimal body) throws WebClientResponseException {
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
    private ResponseSpec fakeOuterStringSerializeRequestCreation(@jakarta.annotation.Nullable String body) throws WebClientResponseException {
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
    public Mono<String> fakeOuterStringSerialize(@jakarta.annotation.Nullable String body) throws WebClientResponseException {
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
    public Mono<ResponseEntity<String>> fakeOuterStringSerializeWithHttpInfo(@jakarta.annotation.Nullable String body) throws WebClientResponseException {
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
    public ResponseSpec fakeOuterStringSerializeWithResponseSpec(@jakarta.annotation.Nullable String body) throws WebClientResponseException {
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
    private ResponseSpec fakePropertyEnumIntegerSerializeRequestCreation(@jakarta.annotation.Nonnull OuterObjectWithEnumProperty outerObjectWithEnumProperty) throws WebClientResponseException {
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
    public Mono<OuterObjectWithEnumProperty> fakePropertyEnumIntegerSerialize(@jakarta.annotation.Nonnull OuterObjectWithEnumProperty outerObjectWithEnumProperty) throws WebClientResponseException {
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
    public Mono<ResponseEntity<OuterObjectWithEnumProperty>> fakePropertyEnumIntegerSerializeWithHttpInfo(@jakarta.annotation.Nonnull OuterObjectWithEnumProperty outerObjectWithEnumProperty) throws WebClientResponseException {
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
    public ResponseSpec fakePropertyEnumIntegerSerializeWithResponseSpec(@jakarta.annotation.Nonnull OuterObjectWithEnumProperty outerObjectWithEnumProperty) throws WebClientResponseException {
        return fakePropertyEnumIntegerSerializeRequestCreation(outerObjectWithEnumProperty);
    }

    /**
     * test referenced additionalProperties
     * 
     * <p><b>200</b> - successful operation
     * @param requestBody request body
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testAdditionalPropertiesReferenceRequestCreation(@jakarta.annotation.Nonnull Map<String, Object> requestBody) throws WebClientResponseException {
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
    public Mono<Void> testAdditionalPropertiesReference(@jakarta.annotation.Nonnull Map<String, Object> requestBody) throws WebClientResponseException {
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
    public Mono<ResponseEntity<Void>> testAdditionalPropertiesReferenceWithHttpInfo(@jakarta.annotation.Nonnull Map<String, Object> requestBody) throws WebClientResponseException {
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
    public ResponseSpec testAdditionalPropertiesReferenceWithResponseSpec(@jakarta.annotation.Nonnull Map<String, Object> requestBody) throws WebClientResponseException {
        return testAdditionalPropertiesReferenceRequestCreation(requestBody);
    }

    /**
     * 
     * For this test, the body has to be a binary file.
     * <p><b>200</b> - Success
     * @param body image to upload
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testBodyWithBinaryRequestCreation(@jakarta.annotation.Nullable File body) throws WebClientResponseException {
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
    public Mono<Void> testBodyWithBinary(@jakarta.annotation.Nullable File body) throws WebClientResponseException {
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
    public Mono<ResponseEntity<Void>> testBodyWithBinaryWithHttpInfo(@jakarta.annotation.Nullable File body) throws WebClientResponseException {
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
    public ResponseSpec testBodyWithBinaryWithResponseSpec(@jakarta.annotation.Nullable File body) throws WebClientResponseException {
        return testBodyWithBinaryRequestCreation(body);
    }

    /**
     * 
     * For this test, the body for this request must reference a schema named &#x60;File&#x60;.
     * <p><b>200</b> - Success
     * @param fileSchemaTestClass The fileSchemaTestClass parameter
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testBodyWithFileSchemaRequestCreation(@jakarta.annotation.Nonnull FileSchemaTestClass fileSchemaTestClass) throws WebClientResponseException {
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
    public Mono<Void> testBodyWithFileSchema(@jakarta.annotation.Nonnull FileSchemaTestClass fileSchemaTestClass) throws WebClientResponseException {
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
    public Mono<ResponseEntity<Void>> testBodyWithFileSchemaWithHttpInfo(@jakarta.annotation.Nonnull FileSchemaTestClass fileSchemaTestClass) throws WebClientResponseException {
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
    public ResponseSpec testBodyWithFileSchemaWithResponseSpec(@jakarta.annotation.Nonnull FileSchemaTestClass fileSchemaTestClass) throws WebClientResponseException {
        return testBodyWithFileSchemaRequestCreation(fileSchemaTestClass);
    }

    /**
     * 
     * 
     * <p><b>200</b> - Success
     * @param query The query parameter
     * @param user The user parameter
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testBodyWithQueryParamsRequestCreation(@jakarta.annotation.Nonnull String query, @jakarta.annotation.Nonnull User user) throws WebClientResponseException {
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
    public Mono<Void> testBodyWithQueryParams(@jakarta.annotation.Nonnull String query, @jakarta.annotation.Nonnull User user) throws WebClientResponseException {
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
    public Mono<ResponseEntity<Void>> testBodyWithQueryParamsWithHttpInfo(@jakarta.annotation.Nonnull String query, @jakarta.annotation.Nonnull User user) throws WebClientResponseException {
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
    public ResponseSpec testBodyWithQueryParamsWithResponseSpec(@jakarta.annotation.Nonnull String query, @jakarta.annotation.Nonnull User user) throws WebClientResponseException {
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
    private ResponseSpec testClientModelRequestCreation(@jakarta.annotation.Nonnull Client client) throws WebClientResponseException {
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
    public Mono<Client> testClientModel(@jakarta.annotation.Nonnull Client client) throws WebClientResponseException {
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
    public Mono<ResponseEntity<Client>> testClientModelWithHttpInfo(@jakarta.annotation.Nonnull Client client) throws WebClientResponseException {
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
    public ResponseSpec testClientModelWithResponseSpec(@jakarta.annotation.Nonnull Client client) throws WebClientResponseException {
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
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testEndpointParametersRequestCreation(@jakarta.annotation.Nonnull BigDecimal number, @jakarta.annotation.Nonnull Double _double, @jakarta.annotation.Nonnull String patternWithoutDelimiter, @jakarta.annotation.Nonnull byte[] _byte, @jakarta.annotation.Nullable Integer integer, @jakarta.annotation.Nullable Integer int32, @jakarta.annotation.Nullable Long int64, @jakarta.annotation.Nullable Float _float, @jakarta.annotation.Nullable String string, @jakarta.annotation.Nullable File binary, @jakarta.annotation.Nullable LocalDate date, @jakarta.annotation.Nullable OffsetDateTime dateTime, @jakarta.annotation.Nullable String password, @jakarta.annotation.Nullable String paramCallback) throws WebClientResponseException {
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
    public Mono<Void> testEndpointParameters(@jakarta.annotation.Nonnull BigDecimal number, @jakarta.annotation.Nonnull Double _double, @jakarta.annotation.Nonnull String patternWithoutDelimiter, @jakarta.annotation.Nonnull byte[] _byte, @jakarta.annotation.Nullable Integer integer, @jakarta.annotation.Nullable Integer int32, @jakarta.annotation.Nullable Long int64, @jakarta.annotation.Nullable Float _float, @jakarta.annotation.Nullable String string, @jakarta.annotation.Nullable File binary, @jakarta.annotation.Nullable LocalDate date, @jakarta.annotation.Nullable OffsetDateTime dateTime, @jakarta.annotation.Nullable String password, @jakarta.annotation.Nullable String paramCallback) throws WebClientResponseException {
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
    public Mono<ResponseEntity<Void>> testEndpointParametersWithHttpInfo(@jakarta.annotation.Nonnull BigDecimal number, @jakarta.annotation.Nonnull Double _double, @jakarta.annotation.Nonnull String patternWithoutDelimiter, @jakarta.annotation.Nonnull byte[] _byte, @jakarta.annotation.Nullable Integer integer, @jakarta.annotation.Nullable Integer int32, @jakarta.annotation.Nullable Long int64, @jakarta.annotation.Nullable Float _float, @jakarta.annotation.Nullable String string, @jakarta.annotation.Nullable File binary, @jakarta.annotation.Nullable LocalDate date, @jakarta.annotation.Nullable OffsetDateTime dateTime, @jakarta.annotation.Nullable String password, @jakarta.annotation.Nullable String paramCallback) throws WebClientResponseException {
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
    public ResponseSpec testEndpointParametersWithResponseSpec(@jakarta.annotation.Nonnull BigDecimal number, @jakarta.annotation.Nonnull Double _double, @jakarta.annotation.Nonnull String patternWithoutDelimiter, @jakarta.annotation.Nonnull byte[] _byte, @jakarta.annotation.Nullable Integer integer, @jakarta.annotation.Nullable Integer int32, @jakarta.annotation.Nullable Long int64, @jakarta.annotation.Nullable Float _float, @jakarta.annotation.Nullable String string, @jakarta.annotation.Nullable File binary, @jakarta.annotation.Nullable LocalDate date, @jakarta.annotation.Nullable OffsetDateTime dateTime, @jakarta.annotation.Nullable String password, @jakarta.annotation.Nullable String paramCallback) throws WebClientResponseException {
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
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testEnumParametersRequestCreation(@jakarta.annotation.Nullable List<String> enumHeaderStringArray, @jakarta.annotation.Nullable String enumHeaderString, @jakarta.annotation.Nullable List<String> enumQueryStringArray, @jakarta.annotation.Nullable String enumQueryString, @jakarta.annotation.Nullable Integer enumQueryInteger, @jakarta.annotation.Nullable Double enumQueryDouble, @jakarta.annotation.Nullable List<EnumClass> enumQueryModelArray, @jakarta.annotation.Nullable List<String> enumFormStringArray, @jakarta.annotation.Nullable String enumFormString) throws WebClientResponseException {
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
    public Mono<Void> testEnumParameters(@jakarta.annotation.Nullable List<String> enumHeaderStringArray, @jakarta.annotation.Nullable String enumHeaderString, @jakarta.annotation.Nullable List<String> enumQueryStringArray, @jakarta.annotation.Nullable String enumQueryString, @jakarta.annotation.Nullable Integer enumQueryInteger, @jakarta.annotation.Nullable Double enumQueryDouble, @jakarta.annotation.Nullable List<EnumClass> enumQueryModelArray, @jakarta.annotation.Nullable List<String> enumFormStringArray, @jakarta.annotation.Nullable String enumFormString) throws WebClientResponseException {
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
    public Mono<ResponseEntity<Void>> testEnumParametersWithHttpInfo(@jakarta.annotation.Nullable List<String> enumHeaderStringArray, @jakarta.annotation.Nullable String enumHeaderString, @jakarta.annotation.Nullable List<String> enumQueryStringArray, @jakarta.annotation.Nullable String enumQueryString, @jakarta.annotation.Nullable Integer enumQueryInteger, @jakarta.annotation.Nullable Double enumQueryDouble, @jakarta.annotation.Nullable List<EnumClass> enumQueryModelArray, @jakarta.annotation.Nullable List<String> enumFormStringArray, @jakarta.annotation.Nullable String enumFormString) throws WebClientResponseException {
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
    public ResponseSpec testEnumParametersWithResponseSpec(@jakarta.annotation.Nullable List<String> enumHeaderStringArray, @jakarta.annotation.Nullable String enumHeaderString, @jakarta.annotation.Nullable List<String> enumQueryStringArray, @jakarta.annotation.Nullable String enumQueryString, @jakarta.annotation.Nullable Integer enumQueryInteger, @jakarta.annotation.Nullable Double enumQueryDouble, @jakarta.annotation.Nullable List<EnumClass> enumQueryModelArray, @jakarta.annotation.Nullable List<String> enumFormStringArray, @jakarta.annotation.Nullable String enumFormString) throws WebClientResponseException {
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
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testGroupParametersRequestCreation(@jakarta.annotation.Nonnull Integer requiredStringGroup, @jakarta.annotation.Nonnull Boolean requiredBooleanGroup, @jakarta.annotation.Nonnull Long requiredInt64Group, @jakarta.annotation.Nullable Integer stringGroup, @jakarta.annotation.Nullable Boolean booleanGroup, @jakarta.annotation.Nullable Long int64Group) throws WebClientResponseException {
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
    public Mono<Void> testGroupParameters(@jakarta.annotation.Nonnull Integer requiredStringGroup, @jakarta.annotation.Nonnull Boolean requiredBooleanGroup, @jakarta.annotation.Nonnull Long requiredInt64Group, @jakarta.annotation.Nullable Integer stringGroup, @jakarta.annotation.Nullable Boolean booleanGroup, @jakarta.annotation.Nullable Long int64Group) throws WebClientResponseException {
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
    public Mono<ResponseEntity<Void>> testGroupParametersWithHttpInfo(@jakarta.annotation.Nonnull Integer requiredStringGroup, @jakarta.annotation.Nonnull Boolean requiredBooleanGroup, @jakarta.annotation.Nonnull Long requiredInt64Group, @jakarta.annotation.Nullable Integer stringGroup, @jakarta.annotation.Nullable Boolean booleanGroup, @jakarta.annotation.Nullable Long int64Group) throws WebClientResponseException {
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
    public ResponseSpec testGroupParametersWithResponseSpec(@jakarta.annotation.Nonnull Integer requiredStringGroup, @jakarta.annotation.Nonnull Boolean requiredBooleanGroup, @jakarta.annotation.Nonnull Long requiredInt64Group, @jakarta.annotation.Nullable Integer stringGroup, @jakarta.annotation.Nullable Boolean booleanGroup, @jakarta.annotation.Nullable Long int64Group) throws WebClientResponseException {
        return testGroupParametersRequestCreation(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, stringGroup, booleanGroup, int64Group);
    }

    /**
     * test inline additionalProperties
     * 
     * <p><b>200</b> - successful operation
     * @param requestBody request body
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testInlineAdditionalPropertiesRequestCreation(@jakarta.annotation.Nonnull Map<String, String> requestBody) throws WebClientResponseException {
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
    public Mono<Void> testInlineAdditionalProperties(@jakarta.annotation.Nonnull Map<String, String> requestBody) throws WebClientResponseException {
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
    public Mono<ResponseEntity<Void>> testInlineAdditionalPropertiesWithHttpInfo(@jakarta.annotation.Nonnull Map<String, String> requestBody) throws WebClientResponseException {
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
    public ResponseSpec testInlineAdditionalPropertiesWithResponseSpec(@jakarta.annotation.Nonnull Map<String, String> requestBody) throws WebClientResponseException {
        return testInlineAdditionalPropertiesRequestCreation(requestBody);
    }

    /**
     * test inline free-form additionalProperties
     * 
     * <p><b>200</b> - successful operation
     * @param testInlineFreeformAdditionalPropertiesRequest request body
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testInlineFreeformAdditionalPropertiesRequestCreation(@jakarta.annotation.Nonnull TestInlineFreeformAdditionalPropertiesRequest testInlineFreeformAdditionalPropertiesRequest) throws WebClientResponseException {
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
    public Mono<Void> testInlineFreeformAdditionalProperties(@jakarta.annotation.Nonnull TestInlineFreeformAdditionalPropertiesRequest testInlineFreeformAdditionalPropertiesRequest) throws WebClientResponseException {
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
    public Mono<ResponseEntity<Void>> testInlineFreeformAdditionalPropertiesWithHttpInfo(@jakarta.annotation.Nonnull TestInlineFreeformAdditionalPropertiesRequest testInlineFreeformAdditionalPropertiesRequest) throws WebClientResponseException {
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
    public ResponseSpec testInlineFreeformAdditionalPropertiesWithResponseSpec(@jakarta.annotation.Nonnull TestInlineFreeformAdditionalPropertiesRequest testInlineFreeformAdditionalPropertiesRequest) throws WebClientResponseException {
        return testInlineFreeformAdditionalPropertiesRequestCreation(testInlineFreeformAdditionalPropertiesRequest);
    }

    /**
     * test json serialization of form data
     * 
     * <p><b>200</b> - successful operation
     * @param param field1
     * @param param2 field2
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testJsonFormDataRequestCreation(@jakarta.annotation.Nonnull String param, @jakarta.annotation.Nonnull String param2) throws WebClientResponseException {
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
    public Mono<Void> testJsonFormData(@jakarta.annotation.Nonnull String param, @jakarta.annotation.Nonnull String param2) throws WebClientResponseException {
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
    public Mono<ResponseEntity<Void>> testJsonFormDataWithHttpInfo(@jakarta.annotation.Nonnull String param, @jakarta.annotation.Nonnull String param2) throws WebClientResponseException {
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
    public ResponseSpec testJsonFormDataWithResponseSpec(@jakarta.annotation.Nonnull String param, @jakarta.annotation.Nonnull String param2) throws WebClientResponseException {
        return testJsonFormDataRequestCreation(param, param2);
    }

    /**
     * test nullable parent property
     * 
     * <p><b>200</b> - successful operation
     * @param childWithNullable request body
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testNullableRequestCreation(@jakarta.annotation.Nonnull ChildWithNullable childWithNullable) throws WebClientResponseException {
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
    public Mono<Void> testNullable(@jakarta.annotation.Nonnull ChildWithNullable childWithNullable) throws WebClientResponseException {
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
    public Mono<ResponseEntity<Void>> testNullableWithHttpInfo(@jakarta.annotation.Nonnull ChildWithNullable childWithNullable) throws WebClientResponseException {
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
    public ResponseSpec testNullableWithResponseSpec(@jakarta.annotation.Nonnull ChildWithNullable childWithNullable) throws WebClientResponseException {
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
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testQueryParameterCollectionFormatRequestCreation(@jakarta.annotation.Nonnull List<String> pipe, @jakarta.annotation.Nonnull List<String> ioutil, @jakarta.annotation.Nonnull List<String> http, @jakarta.annotation.Nonnull List<String> url, @jakarta.annotation.Nonnull List<String> context, @jakarta.annotation.Nonnull String allowEmpty, @jakarta.annotation.Nullable Map<String, String> language) throws WebClientResponseException {
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
    public Mono<Void> testQueryParameterCollectionFormat(@jakarta.annotation.Nonnull List<String> pipe, @jakarta.annotation.Nonnull List<String> ioutil, @jakarta.annotation.Nonnull List<String> http, @jakarta.annotation.Nonnull List<String> url, @jakarta.annotation.Nonnull List<String> context, @jakarta.annotation.Nonnull String allowEmpty, @jakarta.annotation.Nullable Map<String, String> language) throws WebClientResponseException {
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
    public Mono<ResponseEntity<Void>> testQueryParameterCollectionFormatWithHttpInfo(@jakarta.annotation.Nonnull List<String> pipe, @jakarta.annotation.Nonnull List<String> ioutil, @jakarta.annotation.Nonnull List<String> http, @jakarta.annotation.Nonnull List<String> url, @jakarta.annotation.Nonnull List<String> context, @jakarta.annotation.Nonnull String allowEmpty, @jakarta.annotation.Nullable Map<String, String> language) throws WebClientResponseException {
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
    public ResponseSpec testQueryParameterCollectionFormatWithResponseSpec(@jakarta.annotation.Nonnull List<String> pipe, @jakarta.annotation.Nonnull List<String> ioutil, @jakarta.annotation.Nonnull List<String> http, @jakarta.annotation.Nonnull List<String> url, @jakarta.annotation.Nonnull List<String> context, @jakarta.annotation.Nonnull String allowEmpty, @jakarta.annotation.Nullable Map<String, String> language) throws WebClientResponseException {
        return testQueryParameterCollectionFormatRequestCreation(pipe, ioutil, http, url, context, allowEmpty, language);
    }

    /**
     * test referenced string map
     * 
     * <p><b>200</b> - successful operation
     * @param requestBody request body
     * @throws WebClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testStringMapReferenceRequestCreation(@jakarta.annotation.Nonnull Map<String, String> requestBody) throws WebClientResponseException {
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
    public Mono<Void> testStringMapReference(@jakarta.annotation.Nonnull Map<String, String> requestBody) throws WebClientResponseException {
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
    public Mono<ResponseEntity<Void>> testStringMapReferenceWithHttpInfo(@jakarta.annotation.Nonnull Map<String, String> requestBody) throws WebClientResponseException {
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
    public ResponseSpec testStringMapReferenceWithResponseSpec(@jakarta.annotation.Nonnull Map<String, String> requestBody) throws WebClientResponseException {
        return testStringMapReferenceRequestCreation(requestBody);
    }
}
