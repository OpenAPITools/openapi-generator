package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.openapitools.client.BaseApi;


import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.stream.Collectors;

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

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public class AuthApi extends BaseApi {

    public AuthApi() {
        super(new ApiClient());
    }

    public AuthApi(ApiClient apiClient) {
        super(apiClient);
    }

    /**
     * To test HTTP basic authentication
     * To test HTTP basic authentication
     * <p><b>200</b> - Successful operation
     * @return String
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public String testAuthHttpBasic() throws RestClientException {
        return testAuthHttpBasicWithHttpInfo().getBody();
    }

    /**
     * To test HTTP basic authentication
     * To test HTTP basic authentication
     * <p><b>200</b> - Successful operation
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> testAuthHttpBasicWithHttpInfo() throws RestClientException {
        

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<>();
        final HttpHeaders localVarHeaderParams = new HttpHeaders();
        final MultiValueMap<String, String> localVarCookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> localVarFormParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { 
            "text/plain"
         };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = {  };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] { "http_auth" };

        ParameterizedTypeReference<String> localReturnType = new ParameterizedTypeReference<String>() {};
        return apiClient.invokeAPI("/auth/http/basic", HttpMethod.POST, Collections.<String, Object>emptyMap(), localVarQueryParams, null, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localReturnType);
    }
    /**
     * To test HTTP bearer authentication
     * To test HTTP bearer authentication
     * <p><b>200</b> - Successful operation
     * @return String
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public String testAuthHttpBearer() throws RestClientException {
        return testAuthHttpBearerWithHttpInfo().getBody();
    }

    /**
     * To test HTTP bearer authentication
     * To test HTTP bearer authentication
     * <p><b>200</b> - Successful operation
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> testAuthHttpBearerWithHttpInfo() throws RestClientException {
        

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<>();
        final HttpHeaders localVarHeaderParams = new HttpHeaders();
        final MultiValueMap<String, String> localVarCookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> localVarFormParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { 
            "text/plain"
         };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = {  };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] { "http_bearer_auth" };

        ParameterizedTypeReference<String> localReturnType = new ParameterizedTypeReference<String>() {};
        return apiClient.invokeAPI("/auth/http/bearer", HttpMethod.POST, Collections.<String, Object>emptyMap(), localVarQueryParams, null, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localReturnType);
    }

    @Override
    public <T> ResponseEntity<T> invokeAPI(String url, HttpMethod method, Object request, ParameterizedTypeReference<T> returnType) throws RestClientException {
        String localVarPath = url.replace(apiClient.getBasePath(), "");

        final Map<String, Object> uriVariables = new HashMap<>();
        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<>();
        final HttpHeaders localVarHeaderParams = new HttpHeaders();
        final MultiValueMap<String, String> localVarCookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> localVarFormParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { 
            "text/plain"
         };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = {  };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] { "http_bearer_auth" };

        return apiClient.invokeAPI(localVarPath, method, uriVariables, localVarQueryParams, request, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, returnType);
    }
}
