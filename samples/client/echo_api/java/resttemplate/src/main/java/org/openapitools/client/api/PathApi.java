package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.openapitools.client.BaseApi;

import org.openapitools.client.model.StringEnumRef;

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

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen", comments = "Generator version: 7.14.0-SNAPSHOT")
public class PathApi extends BaseApi {

    public PathApi() {
        super(new ApiClient());
    }

    public PathApi(ApiClient apiClient) {
        super(apiClient);
    }

    /**
     * Test path parameter(s)
     * Test path parameter(s)
     * <p><b>200</b> - Successful operation
     * @param pathString  (required)
     * @param pathInteger  (required)
     * @param enumNonrefStringPath  (required)
     * @param enumRefStringPath  (required)
     * @return String
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public String testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath(String pathString, Integer pathInteger, String enumNonrefStringPath, StringEnumRef enumRefStringPath) throws RestClientException {
        return testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPathWithHttpInfo(pathString, pathInteger, enumNonrefStringPath, enumRefStringPath).getBody();
    }

    /**
     * Test path parameter(s)
     * Test path parameter(s)
     * <p><b>200</b> - Successful operation
     * @param pathString  (required)
     * @param pathInteger  (required)
     * @param enumNonrefStringPath  (required)
     * @param enumRefStringPath  (required)
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPathWithHttpInfo(String pathString, Integer pathInteger, String enumNonrefStringPath, StringEnumRef enumRefStringPath) throws RestClientException {
        Object localVarPostBody = null;
        
        // verify the required parameter 'pathString' is set
        if (pathString == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'pathString' when calling testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath");
        }
        
        // verify the required parameter 'pathInteger' is set
        if (pathInteger == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'pathInteger' when calling testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath");
        }
        
        // verify the required parameter 'enumNonrefStringPath' is set
        if (enumNonrefStringPath == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'enumNonrefStringPath' when calling testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath");
        }
        
        // verify the required parameter 'enumRefStringPath' is set
        if (enumRefStringPath == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'enumRefStringPath' when calling testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath");
        }
        
        // create path and map variables
        final Map<String, Object> uriVariables = new HashMap<String, Object>();
        uriVariables.put("path_string", pathString);
        uriVariables.put("path_integer", pathInteger);
        uriVariables.put("enum_nonref_string_path", enumNonrefStringPath);
        uriVariables.put("enum_ref_string_path", enumRefStringPath);

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders localVarHeaderParams = new HttpHeaders();
        final MultiValueMap<String, String> localVarCookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> localVarFormParams = new LinkedMultiValueMap<String, Object>();

        final String[] localVarAccepts = { 
            "text/plain"
         };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = {  };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<String> localReturnType = new ParameterizedTypeReference<String>() {};
        return apiClient.invokeAPI("/path/string/{path_string}/integer/{path_integer}/{enum_nonref_string_path}/{enum_ref_string_path}", HttpMethod.GET, uriVariables, localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localReturnType);
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
            "text/plain"
         };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = {  };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        return apiClient.invokeAPI(localVarPath, method, uriVariables, localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, returnType);
    }
}
