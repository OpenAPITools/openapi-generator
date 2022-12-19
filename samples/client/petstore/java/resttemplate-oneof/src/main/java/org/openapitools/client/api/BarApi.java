package org.openapitools.client.api;

import org.openapitools.client.ApiClient;

import org.openapitools.client.model.Bar;
import org.openapitools.client.model.BarCreate;

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
@Component("org.openapitools.client.api.BarApi")
public class BarApi {
    private ApiClient apiClient;

    public BarApi() {
        this(new ApiClient());
    }

    @Autowired
    public BarApi(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    public ApiClient getApiClient() {
        return apiClient;
    }

    public void setApiClient(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    /**
     * Create a Bar
     * 
     * <p><b>200</b> - Bar created
     * @param barCreate  (required)
     * @return Bar
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public Bar createBar(BarCreate barCreate) throws RestClientException {
        return createBarWithHttpInfo(barCreate).getBody();
    }

    /**
     * Create a Bar
     * 
     * <p><b>200</b> - Bar created
     * @param barCreate  (required)
     * @return ResponseEntity&lt;Bar&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Bar> createBarWithHttpInfo(BarCreate barCreate) throws RestClientException {
        Object localVarPostBody = barCreate;
        
        // verify the required parameter 'barCreate' is set
        if (barCreate == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'barCreate' when calling createBar");
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

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Bar> localReturnType = new ParameterizedTypeReference<Bar>() {};
        return apiClient.invokeAPI("/bar", HttpMethod.POST, Collections.<String, Object>emptyMap(), localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localReturnType);
    }
}
