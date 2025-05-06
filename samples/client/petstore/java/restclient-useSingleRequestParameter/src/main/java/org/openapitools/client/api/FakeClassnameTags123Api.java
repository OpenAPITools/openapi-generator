package org.openapitools.client.api;

import org.openapitools.client.ApiClient;

import org.openapitools.client.model.Client;

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
public class FakeClassnameTags123Api {
    private ApiClient apiClient;

    public FakeClassnameTags123Api() {
        this(new ApiClient());
    }

    @Autowired
    public FakeClassnameTags123Api(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    public ApiClient getApiClient() {
        return apiClient;
    }

    public void setApiClient(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    /**
     * To test class name in snake case
     * To test class name in snake case
     * <p><b>200</b> - successful operation
     * @param client client model
     * @return Client
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testClassnameRequestCreation(@jakarta.annotation.Nonnull Client client) throws RestClientResponseException {
        Object postBody = client;
        // verify the required parameter 'client' is set
        if (client == null) {
            throw new RestClientResponseException("Missing the required parameter 'client' when calling testClassname", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
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

        String[] localVarAuthNames = new String[] { "api_key_query" };

        ParameterizedTypeReference<Client> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/fake_classname_test", HttpMethod.PATCH, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * To test class name in snake case
     * To test class name in snake case
     * <p><b>200</b> - successful operation
     * @param client client model
     * @return Client
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public Client testClassname(@jakarta.annotation.Nonnull Client client) throws RestClientResponseException {
        ParameterizedTypeReference<Client> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testClassnameRequestCreation(client).body(localVarReturnType);
    }

    /**
     * To test class name in snake case
     * To test class name in snake case
     * <p><b>200</b> - successful operation
     * @param client client model
     * @return ResponseEntity&lt;Client&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Client> testClassnameWithHttpInfo(@jakarta.annotation.Nonnull Client client) throws RestClientResponseException {
        ParameterizedTypeReference<Client> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testClassnameRequestCreation(client).toEntity(localVarReturnType);
    }

    /**
     * To test class name in snake case
     * To test class name in snake case
     * <p><b>200</b> - successful operation
     * @param client client model
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testClassnameWithResponseSpec(@jakarta.annotation.Nonnull Client client) throws RestClientResponseException {
        return testClassnameRequestCreation(client);
    }
}
