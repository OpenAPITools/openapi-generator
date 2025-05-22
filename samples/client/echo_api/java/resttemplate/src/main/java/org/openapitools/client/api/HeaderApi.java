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
public class HeaderApi extends BaseApi {

    public HeaderApi() {
        super(new ApiClient());
    }

    public HeaderApi(ApiClient apiClient) {
        super(apiClient);
    }

    /**
     * Test header parameter(s)
     * Test header parameter(s)
     * <p><b>200</b> - Successful operation
     * @param integerHeader  (optional)
     * @param booleanHeader  (optional)
     * @param stringHeader  (optional)
     * @param enumNonrefStringHeader  (optional)
     * @param enumRefStringHeader  (optional)
     * @return String
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public String testHeaderIntegerBooleanStringEnums(Integer integerHeader, Boolean booleanHeader, String stringHeader, String enumNonrefStringHeader, StringEnumRef enumRefStringHeader) throws RestClientException {
        return testHeaderIntegerBooleanStringEnumsWithHttpInfo(integerHeader, booleanHeader, stringHeader, enumNonrefStringHeader, enumRefStringHeader).getBody();
    }

    /**
     * Test header parameter(s)
     * Test header parameter(s)
     * <p><b>200</b> - Successful operation
     * @param integerHeader  (optional)
     * @param booleanHeader  (optional)
     * @param stringHeader  (optional)
     * @param enumNonrefStringHeader  (optional)
     * @param enumRefStringHeader  (optional)
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> testHeaderIntegerBooleanStringEnumsWithHttpInfo(Integer integerHeader, Boolean booleanHeader, String stringHeader, String enumNonrefStringHeader, StringEnumRef enumRefStringHeader) throws RestClientException {
        Object localVarPostBody = null;
        

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders localVarHeaderParams = new HttpHeaders();
        final MultiValueMap<String, String> localVarCookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> localVarFormParams = new LinkedMultiValueMap<String, Object>();

        if (integerHeader != null)
        localVarHeaderParams.add("integer_header", apiClient.parameterToString(integerHeader));
        if (booleanHeader != null)
        localVarHeaderParams.add("boolean_header", apiClient.parameterToString(booleanHeader));
        if (stringHeader != null)
        localVarHeaderParams.add("string_header", apiClient.parameterToString(stringHeader));
        if (enumNonrefStringHeader != null)
        localVarHeaderParams.add("enum_nonref_string_header", apiClient.parameterToString(enumNonrefStringHeader));
        if (enumRefStringHeader != null)
        localVarHeaderParams.add("enum_ref_string_header", apiClient.parameterToString(enumRefStringHeader));

        final String[] localVarAccepts = { 
            "text/plain"
         };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = {  };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<String> localReturnType = new ParameterizedTypeReference<String>() {};
        return apiClient.invokeAPI("/header/integer/boolean/string/enums", HttpMethod.GET, Collections.<String, Object>emptyMap(), localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localReturnType);
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
