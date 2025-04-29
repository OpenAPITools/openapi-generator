package org.openapitools.client.api;

import org.openapitools.client.ApiClient;

import org.openapitools.client.model.Order;

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
public class StoreApi {
    private ApiClient apiClient;

    public StoreApi() {
        this(new ApiClient());
    }

    @Autowired
    public StoreApi(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    public ApiClient getApiClient() {
        return apiClient;
    }

    public void setApiClient(ApiClient apiClient) {
        this.apiClient = apiClient;
    }

    /**
     * Delete purchase order by ID
     * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
     * <p><b>400</b> - Invalid ID supplied
     * <p><b>404</b> - Order not found
     * @param orderId ID of the order that needs to be deleted
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec deleteOrderRequestCreation(@jakarta.annotation.Nonnull String orderId) throws RestClientResponseException {
        Object postBody = null;
        // verify the required parameter 'orderId' is set
        if (orderId == null) {
            throw new RestClientResponseException("Missing the required parameter 'orderId' when calling deleteOrder", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        pathParams.put("order_id", orderId);

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/store/order/{order_id}", HttpMethod.DELETE, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Delete purchase order by ID
     * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
     * <p><b>400</b> - Invalid ID supplied
     * <p><b>404</b> - Order not found
     * @param orderId ID of the order that needs to be deleted
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public void deleteOrder(@jakarta.annotation.Nonnull String orderId) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        deleteOrderRequestCreation(orderId).body(localVarReturnType);
    }

    /**
     * Delete purchase order by ID
     * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
     * <p><b>400</b> - Invalid ID supplied
     * <p><b>404</b> - Order not found
     * @param orderId ID of the order that needs to be deleted
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Void> deleteOrderWithHttpInfo(@jakarta.annotation.Nonnull String orderId) throws RestClientResponseException {
        ParameterizedTypeReference<Void> localVarReturnType = new ParameterizedTypeReference<>() {};
        return deleteOrderRequestCreation(orderId).toEntity(localVarReturnType);
    }

    /**
     * Delete purchase order by ID
     * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
     * <p><b>400</b> - Invalid ID supplied
     * <p><b>404</b> - Order not found
     * @param orderId ID of the order that needs to be deleted
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec deleteOrderWithResponseSpec(@jakarta.annotation.Nonnull String orderId) throws RestClientResponseException {
        return deleteOrderRequestCreation(orderId);
    }
    /**
     * Returns pet inventories by status
     * Returns a map of status codes to quantities
     * <p><b>200</b> - successful operation
     * @return Map&lt;String, Integer&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec getInventoryRequestCreation() throws RestClientResponseException {
        Object postBody = null;
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
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] { "api_key" };

        ParameterizedTypeReference<Map<String, Integer>> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/store/inventory", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Returns pet inventories by status
     * Returns a map of status codes to quantities
     * <p><b>200</b> - successful operation
     * @return Map&lt;String, Integer&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public Map<String, Integer> getInventory() throws RestClientResponseException {
        ParameterizedTypeReference<Map<String, Integer>> localVarReturnType = new ParameterizedTypeReference<>() {};
        return getInventoryRequestCreation().body(localVarReturnType);
    }

    /**
     * Returns pet inventories by status
     * Returns a map of status codes to quantities
     * <p><b>200</b> - successful operation
     * @return ResponseEntity&lt;Map&lt;String, Integer&gt;&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Map<String, Integer>> getInventoryWithHttpInfo() throws RestClientResponseException {
        ParameterizedTypeReference<Map<String, Integer>> localVarReturnType = new ParameterizedTypeReference<>() {};
        return getInventoryRequestCreation().toEntity(localVarReturnType);
    }

    /**
     * Returns pet inventories by status
     * Returns a map of status codes to quantities
     * <p><b>200</b> - successful operation
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec getInventoryWithResponseSpec() throws RestClientResponseException {
        return getInventoryRequestCreation();
    }
    /**
     * Find purchase order by ID
     * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generate exceptions
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid ID supplied
     * <p><b>404</b> - Order not found
     * @param orderId ID of pet that needs to be fetched
     * @return Order
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec getOrderByIdRequestCreation(@jakarta.annotation.Nonnull Long orderId) throws RestClientResponseException {
        Object postBody = null;
        // verify the required parameter 'orderId' is set
        if (orderId == null) {
            throw new RestClientResponseException("Missing the required parameter 'orderId' when calling getOrderById", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        pathParams.put("order_id", orderId);

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { 
            "application/xml", "application/json"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Order> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/store/order/{order_id}", HttpMethod.GET, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Find purchase order by ID
     * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generate exceptions
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid ID supplied
     * <p><b>404</b> - Order not found
     * @param orderId ID of pet that needs to be fetched
     * @return Order
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public Order getOrderById(@jakarta.annotation.Nonnull Long orderId) throws RestClientResponseException {
        ParameterizedTypeReference<Order> localVarReturnType = new ParameterizedTypeReference<>() {};
        return getOrderByIdRequestCreation(orderId).body(localVarReturnType);
    }

    /**
     * Find purchase order by ID
     * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generate exceptions
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid ID supplied
     * <p><b>404</b> - Order not found
     * @param orderId ID of pet that needs to be fetched
     * @return ResponseEntity&lt;Order&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Order> getOrderByIdWithHttpInfo(@jakarta.annotation.Nonnull Long orderId) throws RestClientResponseException {
        ParameterizedTypeReference<Order> localVarReturnType = new ParameterizedTypeReference<>() {};
        return getOrderByIdRequestCreation(orderId).toEntity(localVarReturnType);
    }

    /**
     * Find purchase order by ID
     * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generate exceptions
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid ID supplied
     * <p><b>404</b> - Order not found
     * @param orderId ID of pet that needs to be fetched
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec getOrderByIdWithResponseSpec(@jakarta.annotation.Nonnull Long orderId) throws RestClientResponseException {
        return getOrderByIdRequestCreation(orderId);
    }
    /**
     * Place an order for a pet
     * 
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid Order
     * @param order order placed for purchasing the pet
     * @return Order
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    private ResponseSpec placeOrderRequestCreation(@jakarta.annotation.Nonnull Order order) throws RestClientResponseException {
        Object postBody = order;
        // verify the required parameter 'order' is set
        if (order == null) {
            throw new RestClientResponseException("Missing the required parameter 'order' when calling placeOrder", HttpStatus.BAD_REQUEST.value(), HttpStatus.BAD_REQUEST.getReasonPhrase(), null, null, null);
        }
        // create path and map variables
        final Map<String, Object> pathParams = new HashMap<>();

        final MultiValueMap<String, String> queryParams = new LinkedMultiValueMap<>();
        final HttpHeaders headerParams = new HttpHeaders();
        final MultiValueMap<String, String> cookieParams = new LinkedMultiValueMap<>();
        final MultiValueMap<String, Object> formParams = new LinkedMultiValueMap<>();

        final String[] localVarAccepts = { 
            "application/xml", "application/json"
        };
        final List<MediaType> localVarAccept = apiClient.selectHeaderAccept(localVarAccepts);
        final String[] localVarContentTypes = { 
            "application/json"
        };
        final MediaType localVarContentType = apiClient.selectHeaderContentType(localVarContentTypes);

        String[] localVarAuthNames = new String[] {  };

        ParameterizedTypeReference<Order> localVarReturnType = new ParameterizedTypeReference<>() {};
        return apiClient.invokeAPI("/store/order", HttpMethod.POST, pathParams, queryParams, postBody, headerParams, cookieParams, formParams, localVarAccept, localVarContentType, localVarAuthNames, localVarReturnType);
    }

    /**
     * Place an order for a pet
     * 
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid Order
     * @param order order placed for purchasing the pet
     * @return Order
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public Order placeOrder(@jakarta.annotation.Nonnull Order order) throws RestClientResponseException {
        ParameterizedTypeReference<Order> localVarReturnType = new ParameterizedTypeReference<>() {};
        return placeOrderRequestCreation(order).body(localVarReturnType);
    }

    /**
     * Place an order for a pet
     * 
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid Order
     * @param order order placed for purchasing the pet
     * @return ResponseEntity&lt;Order&gt;
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseEntity<Order> placeOrderWithHttpInfo(@jakarta.annotation.Nonnull Order order) throws RestClientResponseException {
        ParameterizedTypeReference<Order> localVarReturnType = new ParameterizedTypeReference<>() {};
        return placeOrderRequestCreation(order).toEntity(localVarReturnType);
    }

    /**
     * Place an order for a pet
     * 
     * <p><b>200</b> - successful operation
     * <p><b>400</b> - Invalid Order
     * @param order order placed for purchasing the pet
     * @return ResponseSpec
     * @throws RestClientResponseException if an error occurs while attempting to invoke the API
     */
    public ResponseSpec placeOrderWithResponseSpec(@jakarta.annotation.Nonnull Order order) throws RestClientResponseException {
        return placeOrderRequestCreation(order);
    }
}
