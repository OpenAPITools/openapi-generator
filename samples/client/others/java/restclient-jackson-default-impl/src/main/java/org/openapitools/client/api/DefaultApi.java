package org.openapitools.client.api;

import org.openapitools.client.ApiClient;

import org.openapitools.client.model.Animal;
import org.openapitools.client.model.Beverage;
import org.openapitools.client.model.Fruit;

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

@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen", comments = "Generator version: 7.24.0-SNAPSHOT")
public class DefaultApi {
    private ApiClient apiClient;

    public DefaultApi() {
        this(new ApiClient());
    }

    public DefaultApi(ApiClient apiClient) {
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
     * 
     * <p><b>200</b> - OK
     * @param animal The animal parameter
     * @return Animal
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec createAnimalRequestCreation(@jakarta.annotation.Nonnull Animal animal) throws RestClientResponseException {
        Object postBody = animal;
        // verify the required parameter 'animal' is set
        if (animal == null) {
            throw new RestClientResponseException("Missing the required parameter 'animal' when calling createAnimal", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<>();
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

        ParameterizedTypeReference<Animal> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/animals", HttpMethod.POST, pathParams, localVarQueryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - OK
     * @param animal The animal parameter
     * @return Animal
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public Animal createAnimal(@jakarta.annotation.Nonnull Animal animal) throws RestClientResponseException {
        ParameterizedTypeReference<Animal> localVarReturnType = new ParameterizedTypeReference<>() {};
        return createAnimalRequestCreation(animal).body(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - OK
     * @param animal The animal parameter
     * @return ResponseEntity&lt;Animal&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Animal> createAnimalWithHttpInfo(@jakarta.annotation.Nonnull Animal animal) throws RestClientResponseException {
        ParameterizedTypeReference<Animal> localVarReturnType = new ParameterizedTypeReference<>() {};
        return createAnimalRequestCreation(animal).toEntity(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - OK
     * @param animal The animal parameter
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec createAnimalWithResponseSpec(@jakarta.annotation.Nonnull Animal animal) throws RestClientResponseException {
        return createAnimalRequestCreation(animal);
    }

    /**
     * 
     * 
     * <p><b>200</b> - OK
     * @param beverage The beverage parameter
     * @return Beverage
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec createBeverageRequestCreation(@jakarta.annotation.Nonnull Beverage beverage) throws RestClientResponseException {
        Object postBody = beverage;
        // verify the required parameter 'beverage' is set
        if (beverage == null) {
            throw new RestClientResponseException("Missing the required parameter 'beverage' when calling createBeverage", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<>();
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

        ParameterizedTypeReference<Beverage> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/beverages", HttpMethod.POST, pathParams, localVarQueryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - OK
     * @param beverage The beverage parameter
     * @return Beverage
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public Beverage createBeverage(@jakarta.annotation.Nonnull Beverage beverage) throws RestClientResponseException {
        ParameterizedTypeReference<Beverage> localVarReturnType = new ParameterizedTypeReference<>() {};
        return createBeverageRequestCreation(beverage).body(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - OK
     * @param beverage The beverage parameter
     * @return ResponseEntity&lt;Beverage&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Beverage> createBeverageWithHttpInfo(@jakarta.annotation.Nonnull Beverage beverage) throws RestClientResponseException {
        ParameterizedTypeReference<Beverage> localVarReturnType = new ParameterizedTypeReference<>() {};
        return createBeverageRequestCreation(beverage).toEntity(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - OK
     * @param beverage The beverage parameter
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec createBeverageWithResponseSpec(@jakarta.annotation.Nonnull Beverage beverage) throws RestClientResponseException {
        return createBeverageRequestCreation(beverage);
    }

    /**
     * 
     * 
     * <p><b>200</b> - OK
     * @param fruit The fruit parameter
     * @return Fruit
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec createFruitRequestCreation(@jakarta.annotation.Nonnull Fruit fruit) throws RestClientResponseException {
        Object postBody = fruit;
        // verify the required parameter 'fruit' is set
        if (fruit == null) {
            throw new RestClientResponseException("Missing the required parameter 'fruit' when calling createFruit", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> localVarQueryParams = new LinkedMultiValueMap<>();
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

        ParameterizedTypeReference<Fruit> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/fruits", HttpMethod.POST, pathParams, localVarQueryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - OK
     * @param fruit The fruit parameter
     * @return Fruit
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public Fruit createFruit(@jakarta.annotation.Nonnull Fruit fruit) throws RestClientResponseException {
        ParameterizedTypeReference<Fruit> localVarReturnType = new ParameterizedTypeReference<>() {};
        return createFruitRequestCreation(fruit).body(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - OK
     * @param fruit The fruit parameter
     * @return ResponseEntity&lt;Fruit&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Fruit> createFruitWithHttpInfo(@jakarta.annotation.Nonnull Fruit fruit) throws RestClientResponseException {
        ParameterizedTypeReference<Fruit> localVarReturnType = new ParameterizedTypeReference<>() {};
        return createFruitRequestCreation(fruit).toEntity(localVarReturnType);
    }

    /**
     * 
     * 
     * <p><b>200</b> - OK
     * @param fruit The fruit parameter
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec createFruitWithResponseSpec(@jakarta.annotation.Nonnull Fruit fruit) throws RestClientResponseException {
        return createFruitRequestCreation(fruit);
    }
}
