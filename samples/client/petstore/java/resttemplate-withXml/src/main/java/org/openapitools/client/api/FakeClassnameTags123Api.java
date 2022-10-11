package org.openapitools.client.api;

import org.openapitools.client.ApiClient;

import org.openapitools.client.model.Client;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.core.io.FileSystemResource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen")
@Component("org.openapitools.client.api.FakeClassnameTags123Api")
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
     * @param body client model (required)
     * @return Client
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public Client testClassname(Client body) throws RestClientException {
        return testClassnameWithHttpInfo(body).getBody();
    }

    /**
     * To test class name in snake case
     * To test class name in snake case
     * <p><b>200</b> - successful operation
     * @param body client model (required)
     * @return ResponseEntity&lt;Client&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Client> testClassnameWithHttpInfo(Client body) throws RestClientException {
        Object localVarPostBody = body;
        
        // verify the required parameter 'body' is set
        if (body == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'body' when calling testClassname");
        }
        

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders localVarHeaderParams = new HttpHeaders();
        final MultiValueMap<String, String> localVarCookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> localVarFormParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = { 
            "application/json"
         };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
         };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] { "api_key_query" };

        ParameterizedTypeReference<Client> localReturnType = new ParameterizedTypeReference<Client>() {};
        return apiClient.invokeAPI("/fake_classname_test", HttpMethod.PATCH, Collections.<String, Object>emptyMap(), localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localReturnType);
    }
}
