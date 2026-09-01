package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.openapitools.client.BaseApi;

import org.jspecify.annotations.Nullable;
import org.openapitools.client.model.RequiredAndNullable;

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

@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public class NbleApi extends BaseApi {

    public NbleApi() {
        super(new ApiClient());
    }

    public NbleApi(ApiClient apiClient) {
        super(apiClient);
    }

    /**
     * 
     * 
     * <p><b>0</b> - OK
     * @param mandatory  (required)
     * @param optional  (optional)
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public void nbleGet(String mandatory, String optional) throws RestClientException {
        nbleGetWithHttpInfo(mandatory, optional);
    }

    /**
     * 
     * 
     * <p><b>0</b> - OK
     * @param mandatory  (required)
     * @param optional  (optional)
     * @return ResponseEntity&lt;Void&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> nbleGetWithHttpInfo(String mandatory, String optional) throws RestClientException {
        Object localVarPostBody = null;
        
        // verify the required parameter 'mandatory' is set
        if (mandatory == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'mandatory' when calling nbleGet");
        }
        

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders localVarHeaderParams = new HttpHeaders();
        final MultiValueMap<String, String> localVarCookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> localVarFormParams = new LinkedMultiValueMap<String, Object>();

        localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(null, "optional", optional));
        localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(null, "mandatory", mandatory));
        

        final String[] localVarAccepts = {  };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = {  };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localReturnType = new ParameterizedTypeReference<Void>() {};
        return apiClient.invokeAPI("/nble", HttpMethod.GET, Collections.<String, Object>emptyMap(), localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localReturnType);
    }
    /**
     * 
     * 
     * <p><b>0</b> - response
     * @param requiredAndNullable bodyWithRequiredAndNullableAttributes (required)
     * @return RequiredAndNullable
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public RequiredAndNullable nblePost(RequiredAndNullable requiredAndNullable) throws RestClientException {
        return nblePostWithHttpInfo(requiredAndNullable).getBody();
    }

    /**
     * 
     * 
     * <p><b>0</b> - response
     * @param requiredAndNullable bodyWithRequiredAndNullableAttributes (required)
     * @return ResponseEntity&lt;RequiredAndNullable&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<RequiredAndNullable> nblePostWithHttpInfo(RequiredAndNullable requiredAndNullable) throws RestClientException {
        Object localVarPostBody = requiredAndNullable;
        
        // verify the required parameter 'requiredAndNullable' is set
        if (requiredAndNullable == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'requiredAndNullable' when calling nblePost");
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

        ParameterizedTypeReference<RequiredAndNullable> localReturnType = new ParameterizedTypeReference<RequiredAndNullable>() {};
        return apiClient.invokeAPI("/nble", HttpMethod.POST, Collections.<String, Object>emptyMap(), localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localReturnType);
    }

    @Override
    public <T> ResponseEntity<T> invokeAPI(String url, HttpMethod method, Object request, ParameterizedTypeReference<T> returnType) throws RestClientException {
        String localVarPath = url.replace(apiClient.getBasePath(), "");
        Object localVarPostBody = request;

        final Map<String, Object> uriVariables = new HashMap<String, Object>();
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

        return apiClient.invokeAPI(localVarPath, method, uriVariables, localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, returnType);
    }
}
