package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.openapitools.client.BaseApi;

import org.openapitools.client.model.DataQuery;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import org.openapitools.client.model.Pet;
import org.openapitools.client.model.StringEnumRef;
import org.openapitools.client.model.TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter;
import org.openapitools.client.model.TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter;

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

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
public class QueryApi extends BaseApi {

    public QueryApi() {
        super(new ApiClient());
    }

    public QueryApi(ApiClient apiClient) {
        super(apiClient);
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param enumNonrefStringQuery  (optional)
     * @param enumRefStringQuery  (optional)
     * @return String
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public String testEnumRefString(String enumNonrefStringQuery, StringEnumRef enumRefStringQuery) throws RestClientException {
        return testEnumRefStringWithHttpInfo(enumNonrefStringQuery, enumRefStringQuery).getBody();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param enumNonrefStringQuery  (optional)
     * @param enumRefStringQuery  (optional)
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> testEnumRefStringWithHttpInfo(String enumNonrefStringQuery, StringEnumRef enumRefStringQuery) throws RestClientException {
        Object localVarPostBody = null;
        

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders localVarHeaderParams = new HttpHeaders();
        final MultiValueMap<String, String> localVarCookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> localVarFormParams = new LinkedMultiValueMap<String, Object>();

        localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(null, "enum_nonref_string_query", enumNonrefStringQuery));
        localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(null, "enum_ref_string_query", enumRefStringQuery));
        

        final String[] localVarAccepts = { 
            "text/plain"
         };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = {  };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<String> localReturnType = new ParameterizedTypeReference<String>() {};
        return apiClient.invokeAPI("/query/enum_ref_string", HttpMethod.GET, Collections.<String, Object>emptyMap(), localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localReturnType);
    }
    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param datetimeQuery  (optional)
     * @param dateQuery  (optional)
     * @param stringQuery  (optional)
     * @return String
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public String testQueryDatetimeDateString(OffsetDateTime datetimeQuery, LocalDate dateQuery, String stringQuery) throws RestClientException {
        return testQueryDatetimeDateStringWithHttpInfo(datetimeQuery, dateQuery, stringQuery).getBody();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param datetimeQuery  (optional)
     * @param dateQuery  (optional)
     * @param stringQuery  (optional)
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> testQueryDatetimeDateStringWithHttpInfo(OffsetDateTime datetimeQuery, LocalDate dateQuery, String stringQuery) throws RestClientException {
        Object localVarPostBody = null;
        

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders localVarHeaderParams = new HttpHeaders();
        final MultiValueMap<String, String> localVarCookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> localVarFormParams = new LinkedMultiValueMap<String, Object>();

        localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(null, "datetime_query", datetimeQuery));
        localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(null, "date_query", dateQuery));
        localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(null, "string_query", stringQuery));
        

        final String[] localVarAccepts = { 
            "text/plain"
         };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = {  };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<String> localReturnType = new ParameterizedTypeReference<String>() {};
        return apiClient.invokeAPI("/query/datetime/date/string", HttpMethod.GET, Collections.<String, Object>emptyMap(), localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localReturnType);
    }
    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param integerQuery  (optional)
     * @param booleanQuery  (optional)
     * @param stringQuery  (optional)
     * @return String
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public String testQueryIntegerBooleanString(Integer integerQuery, Boolean booleanQuery, String stringQuery) throws RestClientException {
        return testQueryIntegerBooleanStringWithHttpInfo(integerQuery, booleanQuery, stringQuery).getBody();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param integerQuery  (optional)
     * @param booleanQuery  (optional)
     * @param stringQuery  (optional)
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> testQueryIntegerBooleanStringWithHttpInfo(Integer integerQuery, Boolean booleanQuery, String stringQuery) throws RestClientException {
        Object localVarPostBody = null;
        

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders localVarHeaderParams = new HttpHeaders();
        final MultiValueMap<String, String> localVarCookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> localVarFormParams = new LinkedMultiValueMap<String, Object>();

        localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(null, "integer_query", integerQuery));
        localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(null, "boolean_query", booleanQuery));
        localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(null, "string_query", stringQuery));
        

        final String[] localVarAccepts = { 
            "text/plain"
         };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = {  };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<String> localReturnType = new ParameterizedTypeReference<String>() {};
        return apiClient.invokeAPI("/query/integer/boolean/string", HttpMethod.GET, Collections.<String, Object>emptyMap(), localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localReturnType);
    }
    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject  (optional)
     * @return String
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public String testQueryStyleDeepObjectExplodeTrueObject(Pet queryObject) throws RestClientException {
        return testQueryStyleDeepObjectExplodeTrueObjectWithHttpInfo(queryObject).getBody();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject  (optional)
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> testQueryStyleDeepObjectExplodeTrueObjectWithHttpInfo(Pet queryObject) throws RestClientException {
        Object localVarPostBody = null;
        

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders localVarHeaderParams = new HttpHeaders();
        final MultiValueMap<String, String> localVarCookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> localVarFormParams = new LinkedMultiValueMap<String, Object>();

        
        if (queryObject != null) {
            localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(null, "id", queryObject.getId()));
            localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(null, "name", queryObject.getName()));
            localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(null, "category", queryObject.getCategory()));
            localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(null, "photoUrls", queryObject.getPhotoUrls()));
            localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(null, "tags", queryObject.getTags()));
            localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(null, "status", queryObject.getStatus()));
        }

        final String[] localVarAccepts = { 
            "text/plain"
         };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = {  };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<String> localReturnType = new ParameterizedTypeReference<String>() {};
        return apiClient.invokeAPI("/query/style_deepObject/explode_true/object", HttpMethod.GET, Collections.<String, Object>emptyMap(), localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localReturnType);
    }
    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject  (optional)
     * @return String
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public String testQueryStyleDeepObjectExplodeTrueObjectAllOf(TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter queryObject) throws RestClientException {
        return testQueryStyleDeepObjectExplodeTrueObjectAllOfWithHttpInfo(queryObject).getBody();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject  (optional)
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> testQueryStyleDeepObjectExplodeTrueObjectAllOfWithHttpInfo(TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter queryObject) throws RestClientException {
        Object localVarPostBody = null;
        

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders localVarHeaderParams = new HttpHeaders();
        final MultiValueMap<String, String> localVarCookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> localVarFormParams = new LinkedMultiValueMap<String, Object>();

        localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(null, "query_object", queryObject));
        

        final String[] localVarAccepts = { 
            "text/plain"
         };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = {  };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<String> localReturnType = new ParameterizedTypeReference<String>() {};
        return apiClient.invokeAPI("/query/style_deepObject/explode_true/object/allOf", HttpMethod.GET, Collections.<String, Object>emptyMap(), localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localReturnType);
    }
    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject  (optional)
     * @return String
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public String testQueryStyleFormExplodeFalseArrayInteger(List<Integer> queryObject) throws RestClientException {
        return testQueryStyleFormExplodeFalseArrayIntegerWithHttpInfo(queryObject).getBody();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject  (optional)
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> testQueryStyleFormExplodeFalseArrayIntegerWithHttpInfo(List<Integer> queryObject) throws RestClientException {
        Object localVarPostBody = null;
        

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders localVarHeaderParams = new HttpHeaders();
        final MultiValueMap<String, String> localVarCookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> localVarFormParams = new LinkedMultiValueMap<String, Object>();

        localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(ApiClient.CollectionFormat.valueOf("csv".toUpperCase(Locale.ROOT)), "query_object", queryObject));
        

        final String[] localVarAccepts = { 
            "text/plain"
         };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = {  };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<String> localReturnType = new ParameterizedTypeReference<String>() {};
        return apiClient.invokeAPI("/query/style_form/explode_false/array_integer", HttpMethod.GET, Collections.<String, Object>emptyMap(), localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localReturnType);
    }
    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject  (optional)
     * @return String
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public String testQueryStyleFormExplodeFalseArrayString(List<String> queryObject) throws RestClientException {
        return testQueryStyleFormExplodeFalseArrayStringWithHttpInfo(queryObject).getBody();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject  (optional)
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> testQueryStyleFormExplodeFalseArrayStringWithHttpInfo(List<String> queryObject) throws RestClientException {
        Object localVarPostBody = null;
        

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders localVarHeaderParams = new HttpHeaders();
        final MultiValueMap<String, String> localVarCookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> localVarFormParams = new LinkedMultiValueMap<String, Object>();

        localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(ApiClient.CollectionFormat.valueOf("csv".toUpperCase(Locale.ROOT)), "query_object", queryObject));
        

        final String[] localVarAccepts = { 
            "text/plain"
         };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = {  };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<String> localReturnType = new ParameterizedTypeReference<String>() {};
        return apiClient.invokeAPI("/query/style_form/explode_false/array_string", HttpMethod.GET, Collections.<String, Object>emptyMap(), localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localReturnType);
    }
    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject  (optional)
     * @return String
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public String testQueryStyleFormExplodeTrueArrayString(TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter queryObject) throws RestClientException {
        return testQueryStyleFormExplodeTrueArrayStringWithHttpInfo(queryObject).getBody();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject  (optional)
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> testQueryStyleFormExplodeTrueArrayStringWithHttpInfo(TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter queryObject) throws RestClientException {
        Object localVarPostBody = null;
        

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders localVarHeaderParams = new HttpHeaders();
        final MultiValueMap<String, String> localVarCookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> localVarFormParams = new LinkedMultiValueMap<String, Object>();

        
        if (queryObject != null) {
            localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(null, "values", queryObject.getValues()));
        }

        final String[] localVarAccepts = { 
            "text/plain"
         };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = {  };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<String> localReturnType = new ParameterizedTypeReference<String>() {};
        return apiClient.invokeAPI("/query/style_form/explode_true/array_string", HttpMethod.GET, Collections.<String, Object>emptyMap(), localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localReturnType);
    }
    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject  (optional)
     * @return String
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public String testQueryStyleFormExplodeTrueObject(Pet queryObject) throws RestClientException {
        return testQueryStyleFormExplodeTrueObjectWithHttpInfo(queryObject).getBody();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject  (optional)
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> testQueryStyleFormExplodeTrueObjectWithHttpInfo(Pet queryObject) throws RestClientException {
        Object localVarPostBody = null;
        

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders localVarHeaderParams = new HttpHeaders();
        final MultiValueMap<String, String> localVarCookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> localVarFormParams = new LinkedMultiValueMap<String, Object>();

        
        if (queryObject != null) {
            localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(null, "id", queryObject.getId()));
            localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(null, "name", queryObject.getName()));
            localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(null, "category", queryObject.getCategory()));
            localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(null, "photoUrls", queryObject.getPhotoUrls()));
            localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(null, "tags", queryObject.getTags()));
            localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(null, "status", queryObject.getStatus()));
        }

        final String[] localVarAccepts = { 
            "text/plain"
         };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = {  };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<String> localReturnType = new ParameterizedTypeReference<String>() {};
        return apiClient.invokeAPI("/query/style_form/explode_true/object", HttpMethod.GET, Collections.<String, Object>emptyMap(), localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localReturnType);
    }
    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject  (optional)
     * @return String
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public String testQueryStyleFormExplodeTrueObjectAllOf(DataQuery queryObject) throws RestClientException {
        return testQueryStyleFormExplodeTrueObjectAllOfWithHttpInfo(queryObject).getBody();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param queryObject  (optional)
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> testQueryStyleFormExplodeTrueObjectAllOfWithHttpInfo(DataQuery queryObject) throws RestClientException {
        Object localVarPostBody = null;
        

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders localVarHeaderParams = new HttpHeaders();
        final MultiValueMap<String, String> localVarCookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> localVarFormParams = new LinkedMultiValueMap<String, Object>();

        localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(null, "query_object", queryObject));
        

        final String[] localVarAccepts = { 
            "text/plain"
         };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = {  };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<String> localReturnType = new ParameterizedTypeReference<String>() {};
        return apiClient.invokeAPI("/query/style_form/explode_true/object/allOf", HttpMethod.GET, Collections.<String, Object>emptyMap(), localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localReturnType);
    }
    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param jsonSerializedObjectRefStringQuery  (optional)
     * @param jsonSerializedObjectArrayRefStringQuery  (optional)
     * @return String
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public String testQueryStyleJsonSerializationObject(Pet jsonSerializedObjectRefStringQuery, List<Pet> jsonSerializedObjectArrayRefStringQuery) throws RestClientException {
        return testQueryStyleJsonSerializationObjectWithHttpInfo(jsonSerializedObjectRefStringQuery, jsonSerializedObjectArrayRefStringQuery).getBody();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * <p><b>200</b> - Successful operation
     * @param jsonSerializedObjectRefStringQuery  (optional)
     * @param jsonSerializedObjectArrayRefStringQuery  (optional)
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> testQueryStyleJsonSerializationObjectWithHttpInfo(Pet jsonSerializedObjectRefStringQuery, List<Pet> jsonSerializedObjectArrayRefStringQuery) throws RestClientException {
        Object localVarPostBody = null;
        

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<String, String>();
        final HttpHeaders localVarHeaderParams = new HttpHeaders();
        final MultiValueMap<String, String> localVarCookieParams = new LinkedMultiValueMap<String, String>();
        final MultiValueMap<String, Object> localVarFormParams = new LinkedMultiValueMap<String, Object>();

        localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(null, "json_serialized_object_ref_string_query", jsonSerializedObjectRefStringQuery));
        localVarQueryParams.putAll(apiClient.parameterToMultiValueMap(ApiClient.CollectionFormat.valueOf("csv".toUpperCase(Locale.ROOT)), "json_serialized_object_array_ref_string_query", jsonSerializedObjectArrayRefStringQuery));
        

        final String[] localVarAccepts = { 
            "text/plain"
         };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = {  };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<String> localReturnType = new ParameterizedTypeReference<String>() {};
        return apiClient.invokeAPI("/query/style_jsonSerialization/object", HttpMethod.GET, Collections.<String, Object>emptyMap(), localVarQueryParams, localVarPostBody, localVarHeaderParams, localVarCookieParams, localVarFormParams, localVarAccept, localVarContentType, localVarAuthNames, localReturnType);
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
