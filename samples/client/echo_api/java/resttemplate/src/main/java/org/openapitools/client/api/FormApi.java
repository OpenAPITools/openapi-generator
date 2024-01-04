package org.openapitools.client.api;

import org.openapitools.client.ApiClient;


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
public class FormApi {
    private ApiClient apiClient;

    public FormApi() {
        this(new ApiClient());
    }

    public FormApi(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    public ApiClient getApiClient() {
        return apiClient;
    }

    public void setApiClient(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    /**
     * Test form parameter(s)
     * Test form parameter(s)
     * <p><b>200</b> - Successful operation
     * @param integerForm  (optional)
     * @param booleanForm  (optional)
     * @param stringForm  (optional)
     * @return String
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public String testFormIntegerBooleanString(Integer integerForm, Boolean booleanForm, String stringForm) throws RestClientException {
        return testFormIntegerBooleanStringWithHttpInfo(integerForm, booleanForm, stringForm).getBody();
    }

    /**
     * Test form parameter(s)
     * Test form parameter(s)
     * <p><b>200</b> - Successful operation
     * @param integerForm  (optional)
     * @param booleanForm  (optional)
     * @param stringForm  (optional)
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> testFormIntegerBooleanStringWithHttpInfo(Integer integerForm, Boolean booleanForm, String stringForm) throws RestClientException {
        Object localVarPostBody = null;
        

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders localVarHeaderParams = new HttpHeaders();
        final MultiValueMap<String, String> localVarCookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> localVarFormParams = new LinkedMultiValueMap<String, Object>();

        if (integerForm != null)
            localVarFormParams.add("integer_form", integerForm);
        if (booleanForm != null)
            localVarFormParams.add("boolean_form", booleanForm);
        if (stringForm != null)
            localVarFormParams.add("string_form", stringForm);

        final String[] localVarAccepts = { 
            "text/plain"
         };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/x-www-form-urlencoded"
         };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<String> localReturnType = new ParameterizedTypeReference<String>() {};
        return apiClient.invokeAPI("/form/integer/boolean/string", HttpMethod.POST, Collections.<String, Object>emptyMap(), localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localReturnType);
    }
    /**
     * Test form parameter(s) for oneOf schema
     * Test form parameter(s) for oneOf schema
     * <p><b>200</b> - Successful operation
     * @param form1  (optional)
     * @param form2  (optional)
     * @param form3  (optional)
     * @param form4  (optional)
     * @param id  (optional)
     * @param name  (optional)
     * @return String
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public String testFormOneof(String form1, Integer form2, String form3, Boolean form4, Long id, String name) throws RestClientException {
        return testFormOneofWithHttpInfo(form1, form2, form3, form4, id, name).getBody();
    }

    /**
     * Test form parameter(s) for oneOf schema
     * Test form parameter(s) for oneOf schema
     * <p><b>200</b> - Successful operation
     * @param form1  (optional)
     * @param form2  (optional)
     * @param form3  (optional)
     * @param form4  (optional)
     * @param id  (optional)
     * @param name  (optional)
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> testFormOneofWithHttpInfo(String form1, Integer form2, String form3, Boolean form4, Long id, String name) throws RestClientException {
        Object localVarPostBody = null;
        

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders localVarHeaderParams = new HttpHeaders();
        final MultiValueMap<String, String> localVarCookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> localVarFormParams = new LinkedMultiValueMap<String, Object>();

        if (form1 != null)
            localVarFormParams.add("form1", form1);
        if (form2 != null)
            localVarFormParams.add("form2", form2);
        if (form3 != null)
            localVarFormParams.add("form3", form3);
        if (form4 != null)
            localVarFormParams.add("form4", form4);
        if (id != null)
            localVarFormParams.add("id", id);
        if (name != null)
            localVarFormParams.add("name", name);

        final String[] localVarAccepts = { 
            "text/plain"
         };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/x-www-form-urlencoded"
         };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<String> localReturnType = new ParameterizedTypeReference<String>() {};
        return apiClient.invokeAPI("/form/oneof", HttpMethod.POST, Collections.<String, Object>emptyMap(), localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localReturnType);
    }
}
