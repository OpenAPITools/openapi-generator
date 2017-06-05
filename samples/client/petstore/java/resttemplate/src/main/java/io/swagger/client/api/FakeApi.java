package io.swagger.client.api;

import io.swagger.client.ApiClient;

import java.math.BigDecimal;
import io.swagger.client.model.Client;
import org.threeten.bp.LocalDate;
import org.threeten.bp.OffsetDateTime;
import io.swagger.client.model.OuterComposite;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.util.UriComponentsBuilder;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.core.io.FileSystemResource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;


@Component("io.swagger.client.api.FakeApi")
public class FakeApi {
    private ApiClient apiClient;

    public FakeApi() {
        this(new ApiClient());
    }

    @Autowired
    public FakeApi(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    public ApiClient getApiClient() {
        return apiClient;
    }

    public void setApiClient(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    /**
     * 
     * Test serialization of outer boolean types
     * <p><b>200</b> - Output boolean
     * @param body Input boolean as post body
     * @return Boolean
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public Boolean fakeOuterBooleanSerialize(Boolean body) throws RestClientException {
        Object postBody = body;
        
        String path = UriComponentsBuilder.fromPath("/fake/outer/boolean").build().toUriString();
        
        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] accepts = { };
        final List<MediaType> accept = apiClient.selectHeaderAccept(accepts);
        final String[] contentTypes = { };
        final MediaType contentType = apiClient.selectHeaderContentType(contentTypes);

        String[] authNames = new String[] {  };

        ParameterizedTypeReference<Boolean> returnType = new ParameterizedTypeReference<Boolean>() {};
        return apiClient.invokeAPI(path, HttpMethod.POST, queryParams, postBody, headerParams, formParams, accept, contentType, authNames, returnType);
    }
    /**
     * 
     * Test serialization of object with outer number type
     * <p><b>200</b> - Output composite
     * @param body Input composite as post body
     * @return OuterComposite
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public OuterComposite fakeOuterCompositeSerialize(OuterComposite body) throws RestClientException {
        Object postBody = body;
        
        String path = UriComponentsBuilder.fromPath("/fake/outer/composite").build().toUriString();
        
        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] accepts = { };
        final List<MediaType> accept = apiClient.selectHeaderAccept(accepts);
        final String[] contentTypes = { };
        final MediaType contentType = apiClient.selectHeaderContentType(contentTypes);

        String[] authNames = new String[] {  };

        ParameterizedTypeReference<OuterComposite> returnType = new ParameterizedTypeReference<OuterComposite>() {};
        return apiClient.invokeAPI(path, HttpMethod.POST, queryParams, postBody, headerParams, formParams, accept, contentType, authNames, returnType);
    }
    /**
     * 
     * Test serialization of outer number types
     * <p><b>200</b> - Output number
     * @param body Input number as post body
     * @return BigDecimal
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public BigDecimal fakeOuterNumberSerialize(BigDecimal body) throws RestClientException {
        Object postBody = body;
        
        String path = UriComponentsBuilder.fromPath("/fake/outer/number").build().toUriString();
        
        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] accepts = { };
        final List<MediaType> accept = apiClient.selectHeaderAccept(accepts);
        final String[] contentTypes = { };
        final MediaType contentType = apiClient.selectHeaderContentType(contentTypes);

        String[] authNames = new String[] {  };

        ParameterizedTypeReference<BigDecimal> returnType = new ParameterizedTypeReference<BigDecimal>() {};
        return apiClient.invokeAPI(path, HttpMethod.POST, queryParams, postBody, headerParams, formParams, accept, contentType, authNames, returnType);
    }
    /**
     * 
     * Test serialization of outer string types
     * <p><b>200</b> - Output string
     * @param body Input string as post body
     * @return String
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public String fakeOuterStringSerialize(String body) throws RestClientException {
        Object postBody = body;
        
        String path = UriComponentsBuilder.fromPath("/fake/outer/string").build().toUriString();
        
        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] accepts = { };
        final List<MediaType> accept = apiClient.selectHeaderAccept(accepts);
        final String[] contentTypes = { };
        final MediaType contentType = apiClient.selectHeaderContentType(contentTypes);

        String[] authNames = new String[] {  };

        ParameterizedTypeReference<String> returnType = new ParameterizedTypeReference<String>() {};
        return apiClient.invokeAPI(path, HttpMethod.POST, queryParams, postBody, headerParams, formParams, accept, contentType, authNames, returnType);
    }
    /**
     * To test \&quot;client\&quot; model
     * To test \&quot;client\&quot; model
     * <p><b>200</b> - successful operation
     * @param body client model
     * @return Client
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public Client testClientModel(Client body) throws RestClientException {
        Object postBody = body;
        
        // verify the required parameter 'body' is set
        if (body == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'body' when calling testClientModel");
        }
        
        String path = UriComponentsBuilder.fromPath("/fake").build().toUriString();
        
        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();

        final String[] accepts = { 
            "application/json"
        };
        final List<MediaType> accept = apiClient.selectHeaderAccept(accepts);
        final String[] contentTypes = { 
            "application/json"
        };
        final MediaType contentType = apiClient.selectHeaderContentType(contentTypes);

        String[] authNames = new String[] {  };

        ParameterizedTypeReference<Client> returnType = new ParameterizedTypeReference<Client>() {};
        return apiClient.invokeAPI(path, HttpMethod.PATCH, queryParams, postBody, headerParams, formParams, accept, contentType, authNames, returnType);
    }
    /**
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     * <p><b>400</b> - Invalid username supplied
     * <p><b>404</b> - User not found
     * @param number None
     * @param _double None
     * @param patternWithoutDelimiter None
     * @param _byte None
     * @param integer None
     * @param int32 None
     * @param int64 None
     * @param _float None
     * @param string None
     * @param binary None
     * @param date None
     * @param dateTime None
     * @param password None
     * @param paramCallback None
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public void testEndpointParameters(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string, byte[] binary, LocalDate date, OffsetDateTime dateTime, String password, String paramCallback) throws RestClientException {
        Object postBody = null;
        
        // verify the required parameter 'number' is set
        if (number == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'number' when calling testEndpointParameters");
        }
        
        // verify the required parameter '_double' is set
        if (_double == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter '_double' when calling testEndpointParameters");
        }
        
        // verify the required parameter 'patternWithoutDelimiter' is set
        if (patternWithoutDelimiter == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'patternWithoutDelimiter' when calling testEndpointParameters");
        }
        
        // verify the required parameter '_byte' is set
        if (_byte == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter '_byte' when calling testEndpointParameters");
        }
        
        String path = UriComponentsBuilder.fromPath("/fake").build().toUriString();
        
        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();
        
        if (integer != null)
            formParams.add("integer", integer);
        if (int32 != null)
            formParams.add("int32", int32);
        if (int64 != null)
            formParams.add("int64", int64);
        if (number != null)
            formParams.add("number", number);
        if (_float != null)
            formParams.add("float", _float);
        if (_double != null)
            formParams.add("double", _double);
        if (string != null)
            formParams.add("string", string);
        if (patternWithoutDelimiter != null)
            formParams.add("pattern_without_delimiter", patternWithoutDelimiter);
        if (_byte != null)
            formParams.add("byte", _byte);
        if (binary != null)
            formParams.add("binary", binary);
        if (date != null)
            formParams.add("date", date);
        if (dateTime != null)
            formParams.add("dateTime", dateTime);
        if (password != null)
            formParams.add("password", password);
        if (paramCallback != null)
            formParams.add("callback", paramCallback);

        final String[] accepts = { 
            "application/xml; charset&#x3D;utf-8", "application/json; charset&#x3D;utf-8"
        };
        final List<MediaType> accept = apiClient.selectHeaderAccept(accepts);
        final String[] contentTypes = { 
            "application/xml; charset&#x3D;utf-8", "application/json; charset&#x3D;utf-8"
        };
        final MediaType contentType = apiClient.selectHeaderContentType(contentTypes);

        String[] authNames = new String[] { "http_basic_test" };

        ParameterizedTypeReference<Void> returnType = new ParameterizedTypeReference<Void>() {};
        apiClient.invokeAPI(path, HttpMethod.POST, queryParams, postBody, headerParams, formParams, accept, contentType, authNames, returnType);
    }
    /**
     * To test enum parameters
     * To test enum parameters
     * <p><b>400</b> - Invalid request
     * <p><b>404</b> - Not found
     * @param enumFormStringArray Form parameter enum test (string array)
     * @param enumFormString Form parameter enum test (string)
     * @param enumHeaderStringArray Header parameter enum test (string array)
     * @param enumHeaderString Header parameter enum test (string)
     * @param enumQueryStringArray Query parameter enum test (string array)
     * @param enumQueryString Query parameter enum test (string)
     * @param enumQueryInteger Query parameter enum test (double)
     * @param enumQueryDouble Query parameter enum test (double)
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public void testEnumParameters(List<String> enumFormStringArray, String enumFormString, List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble) throws RestClientException {
        Object postBody = null;
        
        String path = UriComponentsBuilder.fromPath("/fake").build().toUriString();
        
        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();
        
        queryParams.putAll(apiClient.parameterToMultiValueMap(ApiClient.CollectionFormat.valueOf("csv".toUpperCase()), "enum_query_string_array", enumQueryStringArray));
        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "enum_query_string", enumQueryString));
        queryParams.putAll(apiClient.parameterToMultiValueMap(null, "enum_query_integer", enumQueryInteger));
        
        if (enumHeaderStringArray != null)
        headerParams.add("enum_header_string_array", apiClient.parameterToString(enumHeaderStringArray));
        if (enumHeaderString != null)
        headerParams.add("enum_header_string", apiClient.parameterToString(enumHeaderString));
        
        if (enumFormStringArray != null)
            formParams.add("enum_form_string_array", enumFormStringArray);
        if (enumFormString != null)
            formParams.add("enum_form_string", enumFormString);
        if (enumQueryDouble != null)
            formParams.add("enum_query_double", enumQueryDouble);

        final String[] accepts = { 
            "*/*"
        };
        final List<MediaType> accept = apiClient.selectHeaderAccept(accepts);
        final String[] contentTypes = { 
            "*/*"
        };
        final MediaType contentType = apiClient.selectHeaderContentType(contentTypes);

        String[] authNames = new String[] {  };

        ParameterizedTypeReference<Void> returnType = new ParameterizedTypeReference<Void>() {};
        apiClient.invokeAPI(path, HttpMethod.GET, queryParams, postBody, headerParams, formParams, accept, contentType, authNames, returnType);
    }
    /**
     * test json serialization of form data
     * 
     * <p><b>200</b> - successful operation
     * @param param field1
     * @param param2 field2
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public void testJsonFormData(String param, String param2) throws RestClientException {
        Object postBody = null;
        
        // verify the required parameter 'param' is set
        if (param == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'param' when calling testJsonFormData");
        }
        
        // verify the required parameter 'param2' is set
        if (param2 == null) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST, "Missing the required parameter 'param2' when calling testJsonFormData");
        }
        
        String path = UriComponentsBuilder.fromPath("/fake/jsonFormData").build().toUriString();
        
        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<String, Object>();
        
        if (param != null)
            formParams.add("param", param);
        if (param2 != null)
            formParams.add("param2", param2);

        final String[] accepts = { };
        final List<MediaType> accept = apiClient.selectHeaderAccept(accepts);
        final String[] contentTypes = { 
            "application/json"
        };
        final MediaType contentType = apiClient.selectHeaderContentType(contentTypes);

        String[] authNames = new String[] {  };

        ParameterizedTypeReference<Void> returnType = new ParameterizedTypeReference<Void>() {};
        apiClient.invokeAPI(path, HttpMethod.GET, queryParams, postBody, headerParams, formParams, accept, contentType, authNames, returnType);
    }
}
