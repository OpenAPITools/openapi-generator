package org.openapitools.client.api;

import org.openapitools.client.ApiClient;


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
import org.springframework.web.client.RestClient.ResponseSpec;
import org.springframework.web.client.RestClientResponseException;

@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen", comments = "Generator version: 7.14.0-SNAPSHOT")
public class AuthApi {
    private ApiClient apiClient;

    public AuthApi() {
        this(new ApiClient());
    }

    @Autowired
    public AuthApi(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    public ApiClient getApiClient() {
        return apiClient;
    }

    public void setApiClient(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    /**
     * To test HTTP basic authentication
     * To test HTTP basic authentication
     * <p><b>200</b> - Successful operation
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testAuthHttpBasicRequestCreation() throws RestClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { 
            "text/plain"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] { "http_auth" };

        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/auth/http/basic", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * To test HTTP basic authentication
     * To test HTTP basic authentication
     * <p><b>200</b> - Successful operation
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public String testAuthHttpBasic() throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testAuthHttpBasicRequestCreation().body(localVarReturnType);
    }

    /**
     * To test HTTP basic authentication
     * To test HTTP basic authentication
     * <p><b>200</b> - Successful operation
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> testAuthHttpBasicWithHttpInfo() throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testAuthHttpBasicRequestCreation().toEntity(localVarReturnType);
    }

    /**
     * To test HTTP basic authentication
     * To test HTTP basic authentication
     * <p><b>200</b> - Successful operation
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testAuthHttpBasicWithResponseSpec() throws RestClientResponseException {
        return testAuthHttpBasicRequestCreation();
    }
    /**
     * To test HTTP bearer authentication
     * To test HTTP bearer authentication
     * <p><b>200</b> - Successful operation
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testAuthHttpBearerRequestCreation() throws RestClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { 
            "text/plain"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] { "http_bearer_auth" };

        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/auth/http/bearer", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * To test HTTP bearer authentication
     * To test HTTP bearer authentication
     * <p><b>200</b> - Successful operation
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public String testAuthHttpBearer() throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testAuthHttpBearerRequestCreation().body(localVarReturnType);
    }

    /**
     * To test HTTP bearer authentication
     * To test HTTP bearer authentication
     * <p><b>200</b> - Successful operation
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> testAuthHttpBearerWithHttpInfo() throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testAuthHttpBearerRequestCreation().toEntity(localVarReturnType);
    }

    /**
     * To test HTTP bearer authentication
     * To test HTTP bearer authentication
     * <p><b>200</b> - Successful operation
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testAuthHttpBearerWithResponseSpec() throws RestClientResponseException {
        return testAuthHttpBearerRequestCreation();
    }
}
