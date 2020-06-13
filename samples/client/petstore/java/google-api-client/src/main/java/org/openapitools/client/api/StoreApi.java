package org.openapitools.client.api;

import org.openapitools.client.ApiClient;

import org.openapitools.client.model.Order;

import com.fasterxml.jackson.core.type.TypeReference;
import com.google.api.client.http.EmptyContent;
import com.google.api.client.http.GenericUrl;
import com.google.api.client.http.HttpContent;
import com.google.api.client.http.InputStreamContent;
import com.google.api.client.http.HttpMethods;
import com.google.api.client.http.HttpResponse;
import com.google.api.client.json.Json;

import javax.ws.rs.core.UriBuilder;
import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.List;
import java.util.ArrayList;

@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaClientCodegen")
public class StoreApi {
    private ApiClient apiClient;

    public StoreApi() {
        this(new ApiClient());
    }

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
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public void deleteOrder(String orderId) throws IOException {
        deleteOrderForHttpResponse(orderId);
    }

  /**
    * Delete purchase order by ID
    * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
    * <p><b>400</b> - Invalid ID supplied
    * <p><b>404</b> - Order not found
    * @param orderId ID of the order that needs to be deleted
    * @param params Map of query params. A collection will be interpreted as passing in multiple instances of the same query param.
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public void deleteOrder(String orderId, Map<String, Object> params) throws IOException {
        deleteOrderForHttpResponse(orderId, params);
    }

    public HttpResponse deleteOrderForHttpResponse(String orderId) throws IOException {
        // verify the required parameter 'orderId' is set
        if (orderId == null) {
            throw new IllegalArgumentException("Missing the required parameter 'orderId' when calling deleteOrder");
        }
        // create a map of path variables
        final Map<String, Object> uriVariables = new HashMap<String, Object>();
        uriVariables.put("order_id", orderId);
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/store/order/{order_id}");

        String localVarUrl = uriBuilder.buildFromMap(uriVariables).toString();
        GenericUrl genericUrl = new GenericUrl(localVarUrl);

        HttpContent content = null;
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.DELETE, genericUrl, content).execute();
    }

    public HttpResponse deleteOrderForHttpResponse(String orderId, Map<String, Object> params) throws IOException {
        // verify the required parameter 'orderId' is set
        if (orderId == null) {
            throw new IllegalArgumentException("Missing the required parameter 'orderId' when calling deleteOrder");
        }
        // create a map of path variables
        final Map<String, Object> uriVariables = new HashMap<String, Object>();
        uriVariables.put("order_id", orderId);
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/store/order/{order_id}");

        // Copy the params argument if present, to allow passing in immutable maps
        Map<String, Object> allParams = params == null ? new HashMap<String, Object>() : new HashMap<String, Object>(params);

        for (Map.Entry<String, Object> entry: allParams.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();

            if (key != null && value != null) {
                if (value instanceof Collection) {
                    uriBuilder = uriBuilder.queryParam(key, ((Collection) value).toArray());
                } else if (value instanceof Object[]) {
                    uriBuilder = uriBuilder.queryParam(key, (Object[]) value);
                } else {
                    uriBuilder = uriBuilder.queryParam(key, value);
                }
            }
        }

        String localVarUrl = uriBuilder.buildFromMap(uriVariables).toString();
        GenericUrl genericUrl = new GenericUrl(localVarUrl);

        HttpContent content = null;
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.DELETE, genericUrl, content).execute();
    }


  /**
    * Returns pet inventories by status
    * Returns a map of status codes to quantities
    * <p><b>200</b> - successful operation
    * @return Map&lt;String, Integer&gt;
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public Map<String, Integer> getInventory() throws IOException {
        HttpResponse response = getInventoryForHttpResponse();
        TypeReference<Map<String, Integer>> typeRef = new TypeReference<Map<String, Integer>>() {};
        return apiClient.getObjectMapper().readValue(response.getContent(), typeRef);
    }

  /**
    * Returns pet inventories by status
    * Returns a map of status codes to quantities
    * <p><b>200</b> - successful operation
    * @param params Map of query params. A collection will be interpreted as passing in multiple instances of the same query param.
    * @return Map&lt;String, Integer&gt;
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public Map<String, Integer> getInventory(Map<String, Object> params) throws IOException {
        HttpResponse response = getInventoryForHttpResponse(params);
        TypeReference<Map<String, Integer>> typeRef = new TypeReference<Map<String, Integer>>() {};
        return apiClient.getObjectMapper().readValue(response.getContent(), typeRef);
    }

    public HttpResponse getInventoryForHttpResponse() throws IOException {
        
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/store/inventory");

        String localVarUrl = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(localVarUrl);

        HttpContent content = null;
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.GET, genericUrl, content).execute();
    }

    public HttpResponse getInventoryForHttpResponse(Map<String, Object> params) throws IOException {
        
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/store/inventory");

        // Copy the params argument if present, to allow passing in immutable maps
        Map<String, Object> allParams = params == null ? new HashMap<String, Object>() : new HashMap<String, Object>(params);

        for (Map.Entry<String, Object> entry: allParams.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();

            if (key != null && value != null) {
                if (value instanceof Collection) {
                    uriBuilder = uriBuilder.queryParam(key, ((Collection) value).toArray());
                } else if (value instanceof Object[]) {
                    uriBuilder = uriBuilder.queryParam(key, (Object[]) value);
                } else {
                    uriBuilder = uriBuilder.queryParam(key, value);
                }
            }
        }

        String localVarUrl = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(localVarUrl);

        HttpContent content = null;
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.GET, genericUrl, content).execute();
    }


  /**
    * Find purchase order by ID
    * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
    * <p><b>200</b> - successful operation
    * <p><b>400</b> - Invalid ID supplied
    * <p><b>404</b> - Order not found
    * @param orderId ID of pet that needs to be fetched
    * @return Order
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public Order getOrderById(Long orderId) throws IOException {
        HttpResponse response = getOrderByIdForHttpResponse(orderId);
        TypeReference<Order> typeRef = new TypeReference<Order>() {};
        return apiClient.getObjectMapper().readValue(response.getContent(), typeRef);
    }

  /**
    * Find purchase order by ID
    * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
    * <p><b>200</b> - successful operation
    * <p><b>400</b> - Invalid ID supplied
    * <p><b>404</b> - Order not found
    * @param orderId ID of pet that needs to be fetched
    * @param params Map of query params. A collection will be interpreted as passing in multiple instances of the same query param.
    * @return Order
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public Order getOrderById(Long orderId, Map<String, Object> params) throws IOException {
        HttpResponse response = getOrderByIdForHttpResponse(orderId, params);
        TypeReference<Order> typeRef = new TypeReference<Order>() {};
        return apiClient.getObjectMapper().readValue(response.getContent(), typeRef);
    }

    public HttpResponse getOrderByIdForHttpResponse(Long orderId) throws IOException {
        // verify the required parameter 'orderId' is set
        if (orderId == null) {
            throw new IllegalArgumentException("Missing the required parameter 'orderId' when calling getOrderById");
        }
        // create a map of path variables
        final Map<String, Object> uriVariables = new HashMap<String, Object>();
        uriVariables.put("order_id", orderId);
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/store/order/{order_id}");

        String localVarUrl = uriBuilder.buildFromMap(uriVariables).toString();
        GenericUrl genericUrl = new GenericUrl(localVarUrl);

        HttpContent content = null;
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.GET, genericUrl, content).execute();
    }

    public HttpResponse getOrderByIdForHttpResponse(Long orderId, Map<String, Object> params) throws IOException {
        // verify the required parameter 'orderId' is set
        if (orderId == null) {
            throw new IllegalArgumentException("Missing the required parameter 'orderId' when calling getOrderById");
        }
        // create a map of path variables
        final Map<String, Object> uriVariables = new HashMap<String, Object>();
        uriVariables.put("order_id", orderId);
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/store/order/{order_id}");

        // Copy the params argument if present, to allow passing in immutable maps
        Map<String, Object> allParams = params == null ? new HashMap<String, Object>() : new HashMap<String, Object>(params);

        for (Map.Entry<String, Object> entry: allParams.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();

            if (key != null && value != null) {
                if (value instanceof Collection) {
                    uriBuilder = uriBuilder.queryParam(key, ((Collection) value).toArray());
                } else if (value instanceof Object[]) {
                    uriBuilder = uriBuilder.queryParam(key, (Object[]) value);
                } else {
                    uriBuilder = uriBuilder.queryParam(key, value);
                }
            }
        }

        String localVarUrl = uriBuilder.buildFromMap(uriVariables).toString();
        GenericUrl genericUrl = new GenericUrl(localVarUrl);

        HttpContent content = null;
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.GET, genericUrl, content).execute();
    }


  /**
    * Place an order for a pet
    * <p><b>200</b> - successful operation
    * <p><b>400</b> - Invalid Order
    * @param body order placed for purchasing the pet
    * @return Order
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public Order placeOrder(Order body) throws IOException {
        HttpResponse response = placeOrderForHttpResponse(body);
        TypeReference<Order> typeRef = new TypeReference<Order>() {};
        return apiClient.getObjectMapper().readValue(response.getContent(), typeRef);
    }

  /**
    * Place an order for a pet
    * <p><b>200</b> - successful operation
    * <p><b>400</b> - Invalid Order
    * @param body order placed for purchasing the pet
    * @param params Map of query params. A collection will be interpreted as passing in multiple instances of the same query param.
    * @return Order
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public Order placeOrder(Order body, Map<String, Object> params) throws IOException {
        HttpResponse response = placeOrderForHttpResponse(body, params);
        TypeReference<Order> typeRef = new TypeReference<Order>() {};
        return apiClient.getObjectMapper().readValue(response.getContent(), typeRef);
    }

    public HttpResponse placeOrderForHttpResponse(Order body) throws IOException {
        // verify the required parameter 'body' is set
        if (body == null) {
            throw new IllegalArgumentException("Missing the required parameter 'body' when calling placeOrder");
        }
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/store/order");

        String localVarUrl = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(localVarUrl);

        HttpContent content = apiClient.new JacksonJsonHttpContent(body);
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.POST, genericUrl, content).execute();
    }

      public HttpResponse placeOrderForHttpResponse(java.io.InputStream body, String mediaType) throws IOException {
          // verify the required parameter 'body' is set
              if (body == null) {
              throw new IllegalArgumentException("Missing the required parameter 'body' when calling placeOrder");
              }
              UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/store/order");

              String localVarUrl = uriBuilder.build().toString();
              GenericUrl genericUrl = new GenericUrl(localVarUrl);

              HttpContent content = body == null ?
                apiClient.new JacksonJsonHttpContent(null) :
                new InputStreamContent(mediaType == null ? Json.MEDIA_TYPE : mediaType, body);
              return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.POST, genericUrl, content).execute();
      }

    public HttpResponse placeOrderForHttpResponse(Order body, Map<String, Object> params) throws IOException {
        // verify the required parameter 'body' is set
        if (body == null) {
            throw new IllegalArgumentException("Missing the required parameter 'body' when calling placeOrder");
        }
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/store/order");

        // Copy the params argument if present, to allow passing in immutable maps
        Map<String, Object> allParams = params == null ? new HashMap<String, Object>() : new HashMap<String, Object>(params);

        for (Map.Entry<String, Object> entry: allParams.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();

            if (key != null && value != null) {
                if (value instanceof Collection) {
                    uriBuilder = uriBuilder.queryParam(key, ((Collection) value).toArray());
                } else if (value instanceof Object[]) {
                    uriBuilder = uriBuilder.queryParam(key, (Object[]) value);
                } else {
                    uriBuilder = uriBuilder.queryParam(key, value);
                }
            }
        }

        String localVarUrl = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(localVarUrl);

        HttpContent content = apiClient.new JacksonJsonHttpContent(body);
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.POST, genericUrl, content).execute();
    }


}
