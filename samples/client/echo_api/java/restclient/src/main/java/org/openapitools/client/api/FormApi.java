package org.openapitools.client.api;

import org.openapitools.client.ApiClient;

import org.openapitools.client.model.TestFormObjectMultipartRequestMarker;

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
public class FormApi {
    private ApiClient apiClient;

    public FormApi() {
        this(new ApiClient());
    }

    @Autowired
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
     * @param integerForm The integerForm parameter
     * @param booleanForm The booleanForm parameter
     * @param stringForm The stringForm parameter
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testFormIntegerBooleanStringRequestCreation(@jakarta.annotation.Nullable Integer integerForm, @jakarta.annotation.Nullable Boolean booleanForm, @jakarta.annotation.Nullable String stringForm) throws RestClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        if (integerForm != null)
            formParams.add("integer_form", integerForm);
        if (booleanForm != null)
            formParams.add("boolean_form", booleanForm);
        if (stringForm != null)
            formParams.add("string_form", stringForm);

        final String[] localVarAccepts = { 
            "text/plain"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/x-www-form-urlencoded"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/form/integer/boolean/string", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Test form parameter(s)
     * Test form parameter(s)
     * <p><b>200</b> - Successful operation
     * @param integerForm The integerForm parameter
     * @param booleanForm The booleanForm parameter
     * @param stringForm The stringForm parameter
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public String testFormIntegerBooleanString(@jakarta.annotation.Nullable Integer integerForm, @jakarta.annotation.Nullable Boolean booleanForm, @jakarta.annotation.Nullable String stringForm) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testFormIntegerBooleanStringRequestCreation(integerForm, booleanForm, stringForm).body(localVarReturnType);
    }

    /**
     * Test form parameter(s)
     * Test form parameter(s)
     * <p><b>200</b> - Successful operation
     * @param integerForm The integerForm parameter
     * @param booleanForm The booleanForm parameter
     * @param stringForm The stringForm parameter
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> testFormIntegerBooleanStringWithHttpInfo(@jakarta.annotation.Nullable Integer integerForm, @jakarta.annotation.Nullable Boolean booleanForm, @jakarta.annotation.Nullable String stringForm) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testFormIntegerBooleanStringRequestCreation(integerForm, booleanForm, stringForm).toEntity(localVarReturnType);
    }

    /**
     * Test form parameter(s)
     * Test form parameter(s)
     * <p><b>200</b> - Successful operation
     * @param integerForm The integerForm parameter
     * @param booleanForm The booleanForm parameter
     * @param stringForm The stringForm parameter
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testFormIntegerBooleanStringWithResponseSpec(@jakarta.annotation.Nullable Integer integerForm, @jakarta.annotation.Nullable Boolean booleanForm, @jakarta.annotation.Nullable String stringForm) throws RestClientResponseException {
        return testFormIntegerBooleanStringRequestCreation(integerForm, booleanForm, stringForm);
    }
    /**
     * Test form parameter(s) for multipart schema
     * Test form parameter(s) for multipart schema
     * <p><b>200</b> - Successful operation
     * @param marker The marker parameter
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testFormObjectMultipartRequestCreation(@jakarta.annotation.Nonnull TestFormObjectMultipartRequestMarker marker) throws RestClientResponseException {
        Object postBody = null;
        // verify the required parameter 'marker' is set
        if (marker == null) {
            throw new RestClientResponseException("Missing the required parameter 'marker' when calling testFormObjectMultipart", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        if (marker != null)
            formParams.add("marker", marker);

        final String[] localVarAccepts = { 
            "text/plain"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "multipart/form-data"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/form/object/multipart", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Test form parameter(s) for multipart schema
     * Test form parameter(s) for multipart schema
     * <p><b>200</b> - Successful operation
     * @param marker The marker parameter
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public String testFormObjectMultipart(@jakarta.annotation.Nonnull TestFormObjectMultipartRequestMarker marker) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testFormObjectMultipartRequestCreation(marker).body(localVarReturnType);
    }

    /**
     * Test form parameter(s) for multipart schema
     * Test form parameter(s) for multipart schema
     * <p><b>200</b> - Successful operation
     * @param marker The marker parameter
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> testFormObjectMultipartWithHttpInfo(@jakarta.annotation.Nonnull TestFormObjectMultipartRequestMarker marker) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testFormObjectMultipartRequestCreation(marker).toEntity(localVarReturnType);
    }

    /**
     * Test form parameter(s) for multipart schema
     * Test form parameter(s) for multipart schema
     * <p><b>200</b> - Successful operation
     * @param marker The marker parameter
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testFormObjectMultipartWithResponseSpec(@jakarta.annotation.Nonnull TestFormObjectMultipartRequestMarker marker) throws RestClientResponseException {
        return testFormObjectMultipartRequestCreation(marker);
    }
    /**
     * Test form parameter(s) for oneOf schema
     * Test form parameter(s) for oneOf schema
     * <p><b>200</b> - Successful operation
     * @param form1 The form1 parameter
     * @param form2 The form2 parameter
     * @param form3 The form3 parameter
     * @param form4 The form4 parameter
     * @param id The id parameter
     * @param name The name parameter
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testFormOneofRequestCreation(@jakarta.annotation.Nullable String form1, @jakarta.annotation.Nullable Integer form2, @jakarta.annotation.Nullable String form3, @jakarta.annotation.Nullable Boolean form4, @jakarta.annotation.Nullable Long id, @jakarta.annotation.Nullable String name) throws RestClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        if (form1 != null)
            formParams.add("form1", form1);
        if (form2 != null)
            formParams.add("form2", form2);
        if (form3 != null)
            formParams.add("form3", form3);
        if (form4 != null)
            formParams.add("form4", form4);
        if (id != null)
            formParams.add("id", id);
        if (name != null)
            formParams.add("name", name);

        final String[] localVarAccepts = { 
            "text/plain"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/x-www-form-urlencoded"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/form/oneof", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Test form parameter(s) for oneOf schema
     * Test form parameter(s) for oneOf schema
     * <p><b>200</b> - Successful operation
     * @param form1 The form1 parameter
     * @param form2 The form2 parameter
     * @param form3 The form3 parameter
     * @param form4 The form4 parameter
     * @param id The id parameter
     * @param name The name parameter
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public String testFormOneof(@jakarta.annotation.Nullable String form1, @jakarta.annotation.Nullable Integer form2, @jakarta.annotation.Nullable String form3, @jakarta.annotation.Nullable Boolean form4, @jakarta.annotation.Nullable Long id, @jakarta.annotation.Nullable String name) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testFormOneofRequestCreation(form1, form2, form3, form4, id, name).body(localVarReturnType);
    }

    /**
     * Test form parameter(s) for oneOf schema
     * Test form parameter(s) for oneOf schema
     * <p><b>200</b> - Successful operation
     * @param form1 The form1 parameter
     * @param form2 The form2 parameter
     * @param form3 The form3 parameter
     * @param form4 The form4 parameter
     * @param id The id parameter
     * @param name The name parameter
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> testFormOneofWithHttpInfo(@jakarta.annotation.Nullable String form1, @jakarta.annotation.Nullable Integer form2, @jakarta.annotation.Nullable String form3, @jakarta.annotation.Nullable Boolean form4, @jakarta.annotation.Nullable Long id, @jakarta.annotation.Nullable String name) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testFormOneofRequestCreation(form1, form2, form3, form4, id, name).toEntity(localVarReturnType);
    }

    /**
     * Test form parameter(s) for oneOf schema
     * Test form parameter(s) for oneOf schema
     * <p><b>200</b> - Successful operation
     * @param form1 The form1 parameter
     * @param form2 The form2 parameter
     * @param form3 The form3 parameter
     * @param form4 The form4 parameter
     * @param id The id parameter
     * @param name The name parameter
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testFormOneofWithResponseSpec(@jakarta.annotation.Nullable String form1, @jakarta.annotation.Nullable Integer form2, @jakarta.annotation.Nullable String form3, @jakarta.annotation.Nullable Boolean form4, @jakarta.annotation.Nullable Long id, @jakarta.annotation.Nullable String name) throws RestClientResponseException {
        return testFormOneofRequestCreation(form1, form2, form3, form4, id, name);
    }
}
