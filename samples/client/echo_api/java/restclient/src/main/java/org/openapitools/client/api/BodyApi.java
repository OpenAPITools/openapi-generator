package org.openapitools.client.api;

import org.openapitools.client.ApiClient;

import java.io.File;
import org.openapitools.client.model.Pet;
import org.openapitools.client.model.StringEnumRef;
import org.openapitools.client.model.Tag;

import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Arrays;
import java.util.stream.Collectors;

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

@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
public class BodyApi {
    private ApiClient apiClient;

    public BodyApi() {
        this(new ApiClient());
    }

    public BodyApi(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    public ApiClient getApiClient() {
        return apiClient;
    }

    public void setApiClient(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    /**
     * Test binary (gif) response body
     * Test binary (gif) response body
     * <p><b>200</b> - Successful operation
     * @return File
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testBinaryGifRequestCreation() throws RestClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { 
            "image/gif"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<File> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/binary/gif", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Test binary (gif) response body
     * Test binary (gif) response body
     * <p><b>200</b> - Successful operation
     * @return File
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public File testBinaryGif() throws RestClientResponseException {
        ParameterizedTypeReference<File> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testBinaryGifRequestCreation().body(localVarReturnType);
    }

    /**
     * Test binary (gif) response body
     * Test binary (gif) response body
     * <p><b>200</b> - Successful operation
     * @return ResponseEntity&lt;File&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<File> testBinaryGifWithHttpInfo() throws RestClientResponseException {
        ParameterizedTypeReference<File> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testBinaryGifRequestCreation().toEntity(localVarReturnType);
    }

    /**
     * Test binary (gif) response body
     * Test binary (gif) response body
     * <p><b>200</b> - Successful operation
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testBinaryGifWithResponseSpec() throws RestClientResponseException {
        return testBinaryGifRequestCreation();
    }

    /**
     * Test body parameter(s)
     * Test body parameter(s)
     * <p><b>200</b> - Successful operation
     * @param body The body parameter
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testBodyApplicationOctetstreamBinaryRequestCreation(@jakarta.annotation.Nullable File body) throws RestClientResponseException {
        Object postBody = body;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { 
            "text/plain"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/octet-stream"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/body/application/octetstream/binary", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Test body parameter(s)
     * Test body parameter(s)
     * <p><b>200</b> - Successful operation
     * @param body The body parameter
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public String testBodyApplicationOctetstreamBinary(@jakarta.annotation.Nullable File body) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testBodyApplicationOctetstreamBinaryRequestCreation(body).body(localVarReturnType);
    }

    /**
     * Test body parameter(s)
     * Test body parameter(s)
     * <p><b>200</b> - Successful operation
     * @param body The body parameter
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> testBodyApplicationOctetstreamBinaryWithHttpInfo(@jakarta.annotation.Nullable File body) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testBodyApplicationOctetstreamBinaryRequestCreation(body).toEntity(localVarReturnType);
    }

    /**
     * Test body parameter(s)
     * Test body parameter(s)
     * <p><b>200</b> - Successful operation
     * @param body The body parameter
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testBodyApplicationOctetstreamBinaryWithResponseSpec(@jakarta.annotation.Nullable File body) throws RestClientResponseException {
        return testBodyApplicationOctetstreamBinaryRequestCreation(body);
    }

    /**
     * Test array of binary in multipart mime
     * Test array of binary in multipart mime
     * <p><b>200</b> - Successful operation
     * @param files The files parameter
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testBodyMultipartFormdataArrayOfBinaryRequestCreation(@jakarta.annotation.Nonnull List<File> files) throws RestClientResponseException {
        Object postBody = null;
        // verify the required parameter 'files' is set
        if (files == null) {
            throw new RestClientResponseException("Missing the required parameter 'files' when calling testBodyMultipartFormdataArrayOfBinary", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        if (files != null)
            formParams.addAll("files", files.stream().map(FileSystemResource::new).collect(Collectors.toList()));

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
        return apiClient.invokeAPI("/body/application/octetstream/array_of_binary", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Test array of binary in multipart mime
     * Test array of binary in multipart mime
     * <p><b>200</b> - Successful operation
     * @param files The files parameter
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public String testBodyMultipartFormdataArrayOfBinary(@jakarta.annotation.Nonnull List<File> files) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testBodyMultipartFormdataArrayOfBinaryRequestCreation(files).body(localVarReturnType);
    }

    /**
     * Test array of binary in multipart mime
     * Test array of binary in multipart mime
     * <p><b>200</b> - Successful operation
     * @param files The files parameter
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> testBodyMultipartFormdataArrayOfBinaryWithHttpInfo(@jakarta.annotation.Nonnull List<File> files) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testBodyMultipartFormdataArrayOfBinaryRequestCreation(files).toEntity(localVarReturnType);
    }

    /**
     * Test array of binary in multipart mime
     * Test array of binary in multipart mime
     * <p><b>200</b> - Successful operation
     * @param files The files parameter
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testBodyMultipartFormdataArrayOfBinaryWithResponseSpec(@jakarta.annotation.Nonnull List<File> files) throws RestClientResponseException {
        return testBodyMultipartFormdataArrayOfBinaryRequestCreation(files);
    }

    /**
     * Test single binary in multipart mime
     * Test single binary in multipart mime
     * <p><b>200</b> - Successful operation
     * @param myFile The myFile parameter
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testBodyMultipartFormdataSingleBinaryRequestCreation(@jakarta.annotation.Nullable File myFile) throws RestClientResponseException {
        Object postBody = null;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        if (myFile != null)
            formParams.add("my-file", new FileSystemResource(myFile));

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
        return apiClient.invokeAPI("/body/application/octetstream/single_binary", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Test single binary in multipart mime
     * Test single binary in multipart mime
     * <p><b>200</b> - Successful operation
     * @param myFile The myFile parameter
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public String testBodyMultipartFormdataSingleBinary(@jakarta.annotation.Nullable File myFile) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testBodyMultipartFormdataSingleBinaryRequestCreation(myFile).body(localVarReturnType);
    }

    /**
     * Test single binary in multipart mime
     * Test single binary in multipart mime
     * <p><b>200</b> - Successful operation
     * @param myFile The myFile parameter
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> testBodyMultipartFormdataSingleBinaryWithHttpInfo(@jakarta.annotation.Nullable File myFile) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testBodyMultipartFormdataSingleBinaryRequestCreation(myFile).toEntity(localVarReturnType);
    }

    /**
     * Test single binary in multipart mime
     * Test single binary in multipart mime
     * <p><b>200</b> - Successful operation
     * @param myFile The myFile parameter
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testBodyMultipartFormdataSingleBinaryWithResponseSpec(@jakarta.annotation.Nullable File myFile) throws RestClientResponseException {
        return testBodyMultipartFormdataSingleBinaryRequestCreation(myFile);
    }

    /**
     * Test body parameter(s)
     * Test body parameter(s)
     * <p><b>200</b> - Successful operation
     * @param pet Pet object that needs to be added to the store
     * @return Pet
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testEchoBodyAllOfPetRequestCreation(@jakarta.annotation.Nullable Pet pet) throws RestClientResponseException {
        Object postBody = pet;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { 
            "application/json"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Pet> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/echo/body/allOf/Pet", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Test body parameter(s)
     * Test body parameter(s)
     * <p><b>200</b> - Successful operation
     * @param pet Pet object that needs to be added to the store
     * @return Pet
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public Pet testEchoBodyAllOfPet(@jakarta.annotation.Nullable Pet pet) throws RestClientResponseException {
        ParameterizedTypeReference<Pet> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testEchoBodyAllOfPetRequestCreation(pet).body(localVarReturnType);
    }

    /**
     * Test body parameter(s)
     * Test body parameter(s)
     * <p><b>200</b> - Successful operation
     * @param pet Pet object that needs to be added to the store
     * @return ResponseEntity&lt;Pet&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Pet> testEchoBodyAllOfPetWithHttpInfo(@jakarta.annotation.Nullable Pet pet) throws RestClientResponseException {
        ParameterizedTypeReference<Pet> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testEchoBodyAllOfPetRequestCreation(pet).toEntity(localVarReturnType);
    }

    /**
     * Test body parameter(s)
     * Test body parameter(s)
     * <p><b>200</b> - Successful operation
     * @param pet Pet object that needs to be added to the store
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testEchoBodyAllOfPetWithResponseSpec(@jakarta.annotation.Nullable Pet pet) throws RestClientResponseException {
        return testEchoBodyAllOfPetRequestCreation(pet);
    }

    /**
     * Test free form object
     * Test free form object
     * <p><b>200</b> - Successful operation
     * @param body Free form object
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testEchoBodyFreeFormObjectResponseStringRequestCreation(@jakarta.annotation.Nullable Object body) throws RestClientResponseException {
        Object postBody = body;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { 
            "text/plain"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/echo/body/FreeFormObject/response_string", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Test free form object
     * Test free form object
     * <p><b>200</b> - Successful operation
     * @param body Free form object
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public String testEchoBodyFreeFormObjectResponseString(@jakarta.annotation.Nullable Object body) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testEchoBodyFreeFormObjectResponseStringRequestCreation(body).body(localVarReturnType);
    }

    /**
     * Test free form object
     * Test free form object
     * <p><b>200</b> - Successful operation
     * @param body Free form object
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> testEchoBodyFreeFormObjectResponseStringWithHttpInfo(@jakarta.annotation.Nullable Object body) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testEchoBodyFreeFormObjectResponseStringRequestCreation(body).toEntity(localVarReturnType);
    }

    /**
     * Test free form object
     * Test free form object
     * <p><b>200</b> - Successful operation
     * @param body Free form object
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testEchoBodyFreeFormObjectResponseStringWithResponseSpec(@jakarta.annotation.Nullable Object body) throws RestClientResponseException {
        return testEchoBodyFreeFormObjectResponseStringRequestCreation(body);
    }

    /**
     * Test body parameter(s)
     * Test body parameter(s)
     * <p><b>200</b> - Successful operation
     * @param pet Pet object that needs to be added to the store
     * @return Pet
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testEchoBodyPetRequestCreation(@jakarta.annotation.Nullable Pet pet) throws RestClientResponseException {
        Object postBody = pet;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { 
            "application/json"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Pet> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/echo/body/Pet", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Test body parameter(s)
     * Test body parameter(s)
     * <p><b>200</b> - Successful operation
     * @param pet Pet object that needs to be added to the store
     * @return Pet
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public Pet testEchoBodyPet(@jakarta.annotation.Nullable Pet pet) throws RestClientResponseException {
        ParameterizedTypeReference<Pet> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testEchoBodyPetRequestCreation(pet).body(localVarReturnType);
    }

    /**
     * Test body parameter(s)
     * Test body parameter(s)
     * <p><b>200</b> - Successful operation
     * @param pet Pet object that needs to be added to the store
     * @return ResponseEntity&lt;Pet&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Pet> testEchoBodyPetWithHttpInfo(@jakarta.annotation.Nullable Pet pet) throws RestClientResponseException {
        ParameterizedTypeReference<Pet> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testEchoBodyPetRequestCreation(pet).toEntity(localVarReturnType);
    }

    /**
     * Test body parameter(s)
     * Test body parameter(s)
     * <p><b>200</b> - Successful operation
     * @param pet Pet object that needs to be added to the store
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testEchoBodyPetWithResponseSpec(@jakarta.annotation.Nullable Pet pet) throws RestClientResponseException {
        return testEchoBodyPetRequestCreation(pet);
    }

    /**
     * Test empty response body
     * Test empty response body
     * <p><b>200</b> - Successful operation
     * @param pet Pet object that needs to be added to the store
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testEchoBodyPetResponseStringRequestCreation(@jakarta.annotation.Nullable Pet pet) throws RestClientResponseException {
        Object postBody = pet;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { 
            "text/plain"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/echo/body/Pet/response_string", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Test empty response body
     * Test empty response body
     * <p><b>200</b> - Successful operation
     * @param pet Pet object that needs to be added to the store
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public String testEchoBodyPetResponseString(@jakarta.annotation.Nullable Pet pet) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testEchoBodyPetResponseStringRequestCreation(pet).body(localVarReturnType);
    }

    /**
     * Test empty response body
     * Test empty response body
     * <p><b>200</b> - Successful operation
     * @param pet Pet object that needs to be added to the store
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> testEchoBodyPetResponseStringWithHttpInfo(@jakarta.annotation.Nullable Pet pet) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testEchoBodyPetResponseStringRequestCreation(pet).toEntity(localVarReturnType);
    }

    /**
     * Test empty response body
     * Test empty response body
     * <p><b>200</b> - Successful operation
     * @param pet Pet object that needs to be added to the store
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testEchoBodyPetResponseStringWithResponseSpec(@jakarta.annotation.Nullable Pet pet) throws RestClientResponseException {
        return testEchoBodyPetResponseStringRequestCreation(pet);
    }

    /**
     * Test string enum response body
     * Test string enum response body
     * <p><b>200</b> - Successful operation
     * @param body String enum
     * @return StringEnumRef
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testEchoBodyStringEnumRequestCreation(@jakarta.annotation.Nullable String body) throws RestClientResponseException {
        Object postBody = body;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { 
            "application/json"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<StringEnumRef> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/echo/body/string_enum", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Test string enum response body
     * Test string enum response body
     * <p><b>200</b> - Successful operation
     * @param body String enum
     * @return StringEnumRef
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public StringEnumRef testEchoBodyStringEnum(@jakarta.annotation.Nullable String body) throws RestClientResponseException {
        ParameterizedTypeReference<StringEnumRef> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testEchoBodyStringEnumRequestCreation(body).body(localVarReturnType);
    }

    /**
     * Test string enum response body
     * Test string enum response body
     * <p><b>200</b> - Successful operation
     * @param body String enum
     * @return ResponseEntity&lt;StringEnumRef&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<StringEnumRef> testEchoBodyStringEnumWithHttpInfo(@jakarta.annotation.Nullable String body) throws RestClientResponseException {
        ParameterizedTypeReference<StringEnumRef> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testEchoBodyStringEnumRequestCreation(body).toEntity(localVarReturnType);
    }

    /**
     * Test string enum response body
     * Test string enum response body
     * <p><b>200</b> - Successful operation
     * @param body String enum
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testEchoBodyStringEnumWithResponseSpec(@jakarta.annotation.Nullable String body) throws RestClientResponseException {
        return testEchoBodyStringEnumRequestCreation(body);
    }

    /**
     * Test empty json (request body)
     * Test empty json (request body)
     * <p><b>200</b> - Successful operation
     * @param tag Tag object
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec testEchoBodyTagResponseStringRequestCreation(@jakarta.annotation.Nullable Tag tag) throws RestClientResponseException {
        Object postBody = tag;
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { 
            "text/plain"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/echo/body/Tag/response_string", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Test empty json (request body)
     * Test empty json (request body)
     * <p><b>200</b> - Successful operation
     * @param tag Tag object
     * @return String
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public String testEchoBodyTagResponseString(@jakarta.annotation.Nullable Tag tag) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testEchoBodyTagResponseStringRequestCreation(tag).body(localVarReturnType);
    }

    /**
     * Test empty json (request body)
     * Test empty json (request body)
     * <p><b>200</b> - Successful operation
     * @param tag Tag object
     * @return ResponseEntity&lt;String&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<String> testEchoBodyTagResponseStringWithHttpInfo(@jakarta.annotation.Nullable Tag tag) throws RestClientResponseException {
        ParameterizedTypeReference<String> localVarReturnType = new ParameterizedTypeReference<>() {};
        return testEchoBodyTagResponseStringRequestCreation(tag).toEntity(localVarReturnType);
    }

    /**
     * Test empty json (request body)
     * Test empty json (request body)
     * <p><b>200</b> - Successful operation
     * @param tag Tag object
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec testEchoBodyTagResponseStringWithResponseSpec(@jakarta.annotation.Nullable Tag tag) throws RestClientResponseException {
        return testEchoBodyTagResponseStringRequestCreation(tag);
    }
}
