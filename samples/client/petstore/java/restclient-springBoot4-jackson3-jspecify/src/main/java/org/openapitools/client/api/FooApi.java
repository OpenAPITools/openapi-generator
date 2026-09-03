package org.openapitools.client.api;

import org.openapitools.client.ApiClient;

import org.openapitools.client.model.Foo;
import org.jspecify.annotations.Nullable;
import java.time.OffsetDateTime;

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
import org.springframework.web.client.RestClient.ResponseSpec;
import org.springframework.web.client.RestClientResponseException;

@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public class FooApi {
    private ApiClient apiClient;

    public FooApi() {
        this(new ApiClient());
    }

    public FooApi(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    public ApiClient getApiClient() {
        return apiClient;
    }

    public void setApiClient(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    public record FooDtParamGetRequest(java.time.@Nullable Instant dtParam, java.time.@Nullable Instant dtQuery, java.time.@Nullable Instant dtCookie, @Nullable String color){}

    /**
     * 
     * 
     * <p><b>0</b> - response
     * @param requestParameters The fooDtParamGet request parameters as object
     * @return Foo
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public Foo fooDtParamGet(FooDtParamGetRequest requestParameters) throws RestClientResponseException {
        return this.fooDtParamGet(requestParameters.dtParam(), requestParameters.dtQuery(), requestParameters.dtCookie(), requestParameters.color());
    }

    /**
     * 
     * 
     * <p><b>0</b> - response
     * @param requestParameters The fooDtParamGet request parameters as object
     * @return ResponseEntity&lt;Foo&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Foo> fooDtParamGetWithHttpInfo(FooDtParamGetRequest requestParameters) throws RestClientResponseException {
        return this.fooDtParamGetWithHttpInfo(requestParameters.dtParam(), requestParameters.dtQuery(), requestParameters.dtCookie(), requestParameters.color());
    }

    /**
     * 
     * 
     * <p><b>0</b> - response
     * @param requestParameters The fooDtParamGet request parameters as object
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec fooDtParamGetWithResponseSpec(FooDtParamGetRequest requestParameters) throws RestClientResponseException {
        return this.fooDtParamGetWithResponseSpec(requestParameters.dtParam(), requestParameters.dtQuery(), requestParameters.dtCookie(), requestParameters.color());
    }

    /**
     * 
     * 
     * <p><b>0</b> - response
     * @param dtParam The dtParam parameter
     * @param dtQuery The dtQuery parameter
     * @param dtCookie The dtCookie parameter
     * @param color The color parameter
     * @return Foo
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec fooDtParamGetRequestCreation(java.time.@Nullable Instant dtParam, java.time.@Nullable Instant dtQuery, java.time.@Nullable Instant dtCookie, @Nullable String color) throws RestClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        pathParams.put("dtParam", dtParam);

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(null, "dtQuery", dtQuery));
        localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(null, "color", color));

        cookieParams.putAll(apiClient.parameterToMultiValueMap(null, "dtCookie", dtCookie));

        final String[] localVarAccepts = { 
            "application/json"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Foo> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/foo/{dtParam}", HttpMethod.GET, pathParams, localVarQueryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>0</b> - response
     * @param dtParam The dtParam parameter
     * @param dtQuery The dtQuery parameter
     * @param dtCookie The dtCookie parameter
     * @param color The color parameter
     * @return Foo
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public Foo fooDtParamGet(java.time.@Nullable Instant dtParam, java.time.@Nullable Instant dtQuery, java.time.@Nullable Instant dtCookie, @Nullable String color) throws RestClientResponseException {
        ParameterizedTypeReference<Foo> localVarReturnType = new ParameterizedTypeReference<>() {};
        return fooDtParamGetRequestCreation(dtParam, dtQuery, dtCookie, color).body(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>0</b> - response
     * @param dtParam The dtParam parameter
     * @param dtQuery The dtQuery parameter
     * @param dtCookie The dtCookie parameter
     * @param color The color parameter
     * @return ResponseEntity&lt;Foo&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Foo> fooDtParamGetWithHttpInfo(java.time.@Nullable Instant dtParam, java.time.@Nullable Instant dtQuery, java.time.@Nullable Instant dtCookie, @Nullable String color) throws RestClientResponseException {
        ParameterizedTypeReference<Foo> localVarReturnType = new ParameterizedTypeReference<>() {};
        return fooDtParamGetRequestCreation(dtParam, dtQuery, dtCookie, color).toEntity(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>0</b> - response
     * @param dtParam The dtParam parameter
     * @param dtQuery The dtQuery parameter
     * @param dtCookie The dtCookie parameter
     * @param color The color parameter
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec fooDtParamGetWithResponseSpec(java.time.@Nullable Instant dtParam, java.time.@Nullable Instant dtQuery, java.time.@Nullable Instant dtCookie, @Nullable String color) throws RestClientResponseException {
        return fooDtParamGetRequestCreation(dtParam, dtQuery, dtCookie, color);
    }
}
