package org.openapitools.client.api;

import org.openapitools.client.ApiClient;

import java.math.BigDecimal;
import org.openapitools.client.model.Client;
import java.io.File;
import org.openapitools.client.model.FileSchemaTestClass;
import org.threeten.bp.LocalDate;
import org.threeten.bp.OffsetDateTime;
import org.openapitools.client.model.OuterComposite;
import org.openapitools.client.model.User;
import org.openapitools.client.model.XmlItem;

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
public class FakeApi {
    private ApiClient apiClient;

    public FakeApi() {
        this(new ApiClient());
    }

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
    * creates an XmlItem
    * this route creates an XmlItem
    * <p><b>200</b> - successful operation
    * @param xmlItem XmlItem Body
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public void createXmlItem(XmlItem xmlItem) throws IOException {
        createXmlItemForHttpResponse(xmlItem);
    }

  /**
    * creates an XmlItem
    * this route creates an XmlItem
    * <p><b>200</b> - successful operation
    * @param xmlItem XmlItem Body
    * @param params Map of query params. A collection will be interpreted as passing in multiple instances of the same query param.
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public void createXmlItem(XmlItem xmlItem, Map<String, Object> params) throws IOException {
        createXmlItemForHttpResponse(xmlItem, params);
    }

    public HttpResponse createXmlItemForHttpResponse(XmlItem xmlItem) throws IOException {
        // verify the required parameter 'xmlItem' is set
        if (xmlItem == null) {
            throw new IllegalArgumentException("Missing the required parameter 'xmlItem' when calling createXmlItem");
        }
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/fake/create_xml_item");

        String localVarUrl = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(localVarUrl);

        HttpContent content = apiClient.new JacksonJsonHttpContent(xmlItem);
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.POST, genericUrl, content).execute();
    }

      public HttpResponse createXmlItemForHttpResponse(java.io.InputStream xmlItem, String mediaType) throws IOException {
          // verify the required parameter 'xmlItem' is set
              if (xmlItem == null) {
              throw new IllegalArgumentException("Missing the required parameter 'xmlItem' when calling createXmlItem");
              }
              UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/fake/create_xml_item");

              String localVarUrl = uriBuilder.build().toString();
              GenericUrl genericUrl = new GenericUrl(localVarUrl);

              HttpContent content = xmlItem == null ?
                apiClient.new JacksonJsonHttpContent(null) :
                new InputStreamContent(mediaType == null ? Json.MEDIA_TYPE : mediaType, xmlItem);
              return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.POST, genericUrl, content).execute();
      }

    public HttpResponse createXmlItemForHttpResponse(XmlItem xmlItem, Map<String, Object> params) throws IOException {
        // verify the required parameter 'xmlItem' is set
        if (xmlItem == null) {
            throw new IllegalArgumentException("Missing the required parameter 'xmlItem' when calling createXmlItem");
        }
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/fake/create_xml_item");

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

        HttpContent content = apiClient.new JacksonJsonHttpContent(xmlItem);
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.POST, genericUrl, content).execute();
    }


  /**
    * Test serialization of outer boolean types
    * <p><b>200</b> - Output boolean
    * @param body Input boolean as post body
    * @return Boolean
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public Boolean fakeOuterBooleanSerialize(Boolean body) throws IOException {
        HttpResponse response = fakeOuterBooleanSerializeForHttpResponse(body);
        TypeReference<Boolean> typeRef = new TypeReference<Boolean>() {};
        return apiClient.getObjectMapper().readValue(response.getContent(), typeRef);
    }

  /**
    * Test serialization of outer boolean types
    * <p><b>200</b> - Output boolean
    * @param params Map of query params. A collection will be interpreted as passing in multiple instances of the same query param.
    * @return Boolean
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public Boolean fakeOuterBooleanSerialize(Boolean body, Map<String, Object> params) throws IOException {
        HttpResponse response = fakeOuterBooleanSerializeForHttpResponse(body, params);
        TypeReference<Boolean> typeRef = new TypeReference<Boolean>() {};
        return apiClient.getObjectMapper().readValue(response.getContent(), typeRef);
    }

    public HttpResponse fakeOuterBooleanSerializeForHttpResponse(Boolean body) throws IOException {
        
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/fake/outer/boolean");

        String localVarUrl = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(localVarUrl);

        HttpContent content = apiClient.new JacksonJsonHttpContent(body);
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.POST, genericUrl, content).execute();
    }

      public HttpResponse fakeOuterBooleanSerializeForHttpResponse(java.io.InputStream body, String mediaType) throws IOException {
          
              UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/fake/outer/boolean");

              String localVarUrl = uriBuilder.build().toString();
              GenericUrl genericUrl = new GenericUrl(localVarUrl);

              HttpContent content = body == null ?
                apiClient.new JacksonJsonHttpContent(null) :
                new InputStreamContent(mediaType == null ? Json.MEDIA_TYPE : mediaType, body);
              return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.POST, genericUrl, content).execute();
      }

    public HttpResponse fakeOuterBooleanSerializeForHttpResponse(Boolean body, Map<String, Object> params) throws IOException {
        
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/fake/outer/boolean");

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


  /**
    * Test serialization of object with outer number type
    * <p><b>200</b> - Output composite
    * @param body Input composite as post body
    * @return OuterComposite
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public OuterComposite fakeOuterCompositeSerialize(OuterComposite body) throws IOException {
        HttpResponse response = fakeOuterCompositeSerializeForHttpResponse(body);
        TypeReference<OuterComposite> typeRef = new TypeReference<OuterComposite>() {};
        return apiClient.getObjectMapper().readValue(response.getContent(), typeRef);
    }

  /**
    * Test serialization of object with outer number type
    * <p><b>200</b> - Output composite
    * @param params Map of query params. A collection will be interpreted as passing in multiple instances of the same query param.
    * @return OuterComposite
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public OuterComposite fakeOuterCompositeSerialize(OuterComposite body, Map<String, Object> params) throws IOException {
        HttpResponse response = fakeOuterCompositeSerializeForHttpResponse(body, params);
        TypeReference<OuterComposite> typeRef = new TypeReference<OuterComposite>() {};
        return apiClient.getObjectMapper().readValue(response.getContent(), typeRef);
    }

    public HttpResponse fakeOuterCompositeSerializeForHttpResponse(OuterComposite body) throws IOException {
        
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/fake/outer/composite");

        String localVarUrl = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(localVarUrl);

        HttpContent content = apiClient.new JacksonJsonHttpContent(body);
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.POST, genericUrl, content).execute();
    }

      public HttpResponse fakeOuterCompositeSerializeForHttpResponse(java.io.InputStream body, String mediaType) throws IOException {
          
              UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/fake/outer/composite");

              String localVarUrl = uriBuilder.build().toString();
              GenericUrl genericUrl = new GenericUrl(localVarUrl);

              HttpContent content = body == null ?
                apiClient.new JacksonJsonHttpContent(null) :
                new InputStreamContent(mediaType == null ? Json.MEDIA_TYPE : mediaType, body);
              return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.POST, genericUrl, content).execute();
      }

    public HttpResponse fakeOuterCompositeSerializeForHttpResponse(OuterComposite body, Map<String, Object> params) throws IOException {
        
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/fake/outer/composite");

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


  /**
    * Test serialization of outer number types
    * <p><b>200</b> - Output number
    * @param body Input number as post body
    * @return BigDecimal
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public BigDecimal fakeOuterNumberSerialize(BigDecimal body) throws IOException {
        HttpResponse response = fakeOuterNumberSerializeForHttpResponse(body);
        TypeReference<BigDecimal> typeRef = new TypeReference<BigDecimal>() {};
        return apiClient.getObjectMapper().readValue(response.getContent(), typeRef);
    }

  /**
    * Test serialization of outer number types
    * <p><b>200</b> - Output number
    * @param params Map of query params. A collection will be interpreted as passing in multiple instances of the same query param.
    * @return BigDecimal
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public BigDecimal fakeOuterNumberSerialize(BigDecimal body, Map<String, Object> params) throws IOException {
        HttpResponse response = fakeOuterNumberSerializeForHttpResponse(body, params);
        TypeReference<BigDecimal> typeRef = new TypeReference<BigDecimal>() {};
        return apiClient.getObjectMapper().readValue(response.getContent(), typeRef);
    }

    public HttpResponse fakeOuterNumberSerializeForHttpResponse(BigDecimal body) throws IOException {
        
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/fake/outer/number");

        String localVarUrl = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(localVarUrl);

        HttpContent content = apiClient.new JacksonJsonHttpContent(body);
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.POST, genericUrl, content).execute();
    }

      public HttpResponse fakeOuterNumberSerializeForHttpResponse(java.io.InputStream body, String mediaType) throws IOException {
          
              UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/fake/outer/number");

              String localVarUrl = uriBuilder.build().toString();
              GenericUrl genericUrl = new GenericUrl(localVarUrl);

              HttpContent content = body == null ?
                apiClient.new JacksonJsonHttpContent(null) :
                new InputStreamContent(mediaType == null ? Json.MEDIA_TYPE : mediaType, body);
              return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.POST, genericUrl, content).execute();
      }

    public HttpResponse fakeOuterNumberSerializeForHttpResponse(BigDecimal body, Map<String, Object> params) throws IOException {
        
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/fake/outer/number");

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


  /**
    * Test serialization of outer string types
    * <p><b>200</b> - Output string
    * @param body Input string as post body
    * @return String
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public String fakeOuterStringSerialize(String body) throws IOException {
        HttpResponse response = fakeOuterStringSerializeForHttpResponse(body);
        TypeReference<String> typeRef = new TypeReference<String>() {};
        return apiClient.getObjectMapper().readValue(response.getContent(), typeRef);
    }

  /**
    * Test serialization of outer string types
    * <p><b>200</b> - Output string
    * @param params Map of query params. A collection will be interpreted as passing in multiple instances of the same query param.
    * @return String
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public String fakeOuterStringSerialize(String body, Map<String, Object> params) throws IOException {
        HttpResponse response = fakeOuterStringSerializeForHttpResponse(body, params);
        TypeReference<String> typeRef = new TypeReference<String>() {};
        return apiClient.getObjectMapper().readValue(response.getContent(), typeRef);
    }

    public HttpResponse fakeOuterStringSerializeForHttpResponse(String body) throws IOException {
        
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/fake/outer/string");

        String localVarUrl = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(localVarUrl);

        HttpContent content = apiClient.new JacksonJsonHttpContent(body);
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.POST, genericUrl, content).execute();
    }

      public HttpResponse fakeOuterStringSerializeForHttpResponse(java.io.InputStream body, String mediaType) throws IOException {
          
              UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/fake/outer/string");

              String localVarUrl = uriBuilder.build().toString();
              GenericUrl genericUrl = new GenericUrl(localVarUrl);

              HttpContent content = body == null ?
                apiClient.new JacksonJsonHttpContent(null) :
                new InputStreamContent(mediaType == null ? Json.MEDIA_TYPE : mediaType, body);
              return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.POST, genericUrl, content).execute();
      }

    public HttpResponse fakeOuterStringSerializeForHttpResponse(String body, Map<String, Object> params) throws IOException {
        
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/fake/outer/string");

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


  /**
    * For this test, the body for this request much reference a schema named &#x60;File&#x60;.
    * <p><b>200</b> - Success
    * @param body The body parameter
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public void testBodyWithFileSchema(FileSchemaTestClass body) throws IOException {
        testBodyWithFileSchemaForHttpResponse(body);
    }

  /**
    * For this test, the body for this request much reference a schema named &#x60;File&#x60;.
    * <p><b>200</b> - Success
    * @param body The body parameter
    * @param params Map of query params. A collection will be interpreted as passing in multiple instances of the same query param.
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public void testBodyWithFileSchema(FileSchemaTestClass body, Map<String, Object> params) throws IOException {
        testBodyWithFileSchemaForHttpResponse(body, params);
    }

    public HttpResponse testBodyWithFileSchemaForHttpResponse(FileSchemaTestClass body) throws IOException {
        // verify the required parameter 'body' is set
        if (body == null) {
            throw new IllegalArgumentException("Missing the required parameter 'body' when calling testBodyWithFileSchema");
        }
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/fake/body-with-file-schema");

        String localVarUrl = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(localVarUrl);

        HttpContent content = apiClient.new JacksonJsonHttpContent(body);
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.PUT, genericUrl, content).execute();
    }

      public HttpResponse testBodyWithFileSchemaForHttpResponse(java.io.InputStream body, String mediaType) throws IOException {
          // verify the required parameter 'body' is set
              if (body == null) {
              throw new IllegalArgumentException("Missing the required parameter 'body' when calling testBodyWithFileSchema");
              }
              UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/fake/body-with-file-schema");

              String localVarUrl = uriBuilder.build().toString();
              GenericUrl genericUrl = new GenericUrl(localVarUrl);

              HttpContent content = body == null ?
                apiClient.new JacksonJsonHttpContent(null) :
                new InputStreamContent(mediaType == null ? Json.MEDIA_TYPE : mediaType, body);
              return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.PUT, genericUrl, content).execute();
      }

    public HttpResponse testBodyWithFileSchemaForHttpResponse(FileSchemaTestClass body, Map<String, Object> params) throws IOException {
        // verify the required parameter 'body' is set
        if (body == null) {
            throw new IllegalArgumentException("Missing the required parameter 'body' when calling testBodyWithFileSchema");
        }
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/fake/body-with-file-schema");

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
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.PUT, genericUrl, content).execute();
    }


  /**
    * <p><b>200</b> - Success
    * @param query The query parameter
    * @param body The body parameter
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public void testBodyWithQueryParams(String query, User body) throws IOException {
        testBodyWithQueryParamsForHttpResponse(query, body);
    }

  /**
    * <p><b>200</b> - Success
    * @param query The query parameter
    * @param body The body parameter
    * @param params Map of query params. A collection will be interpreted as passing in multiple instances of the same query param.
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public void testBodyWithQueryParams(String query, User body, Map<String, Object> params) throws IOException {
        testBodyWithQueryParamsForHttpResponse(query, body, params);
    }

    public HttpResponse testBodyWithQueryParamsForHttpResponse(String query, User body) throws IOException {
        // verify the required parameter 'query' is set
        if (query == null) {
            throw new IllegalArgumentException("Missing the required parameter 'query' when calling testBodyWithQueryParams");
        }// verify the required parameter 'body' is set
        if (body == null) {
            throw new IllegalArgumentException("Missing the required parameter 'body' when calling testBodyWithQueryParams");
        }
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/fake/body-with-query-params");
        if (query != null) {
            String key = "query";
            Object value = query;
            if (value instanceof Collection) {
                uriBuilder = uriBuilder.queryParam(key, ((Collection) value).toArray());
            } else if (value instanceof Object[]) {
                uriBuilder = uriBuilder.queryParam(key, (Object[]) value);
            } else {
                uriBuilder = uriBuilder.queryParam(key, value);
            }
        }

        String localVarUrl = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(localVarUrl);

        HttpContent content = apiClient.new JacksonJsonHttpContent(body);
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.PUT, genericUrl, content).execute();
    }

      public HttpResponse testBodyWithQueryParamsForHttpResponse(String query, java.io.InputStream body, String mediaType) throws IOException {
          // verify the required parameter 'query' is set
              if (query == null) {
              throw new IllegalArgumentException("Missing the required parameter 'query' when calling testBodyWithQueryParams");
              }// verify the required parameter 'body' is set
              if (body == null) {
              throw new IllegalArgumentException("Missing the required parameter 'body' when calling testBodyWithQueryParams");
              }
              UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/fake/body-with-query-params");
              if (query != null) {
                  String key = "query";
                  Object value = query;
                  if (value instanceof Collection) {
                    uriBuilder = uriBuilder.queryParam(key, ((Collection) value).toArray());
                  } else if (value instanceof Object[]) {
                    uriBuilder = uriBuilder.queryParam(key, (Object[]) value);
                  } else {
                    uriBuilder = uriBuilder.queryParam(key, value);
                  }
              }

              String localVarUrl = uriBuilder.build().toString();
              GenericUrl genericUrl = new GenericUrl(localVarUrl);

              HttpContent content = body == null ?
                apiClient.new JacksonJsonHttpContent(null) :
                new InputStreamContent(mediaType == null ? Json.MEDIA_TYPE : mediaType, body);
              return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.PUT, genericUrl, content).execute();
      }

    public HttpResponse testBodyWithQueryParamsForHttpResponse(String query, User body, Map<String, Object> params) throws IOException {
        // verify the required parameter 'query' is set
        if (query == null) {
            throw new IllegalArgumentException("Missing the required parameter 'query' when calling testBodyWithQueryParams");
        }// verify the required parameter 'body' is set
        if (body == null) {
            throw new IllegalArgumentException("Missing the required parameter 'body' when calling testBodyWithQueryParams");
        }
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/fake/body-with-query-params");

        // Copy the params argument if present, to allow passing in immutable maps
        Map<String, Object> allParams = params == null ? new HashMap<String, Object>() : new HashMap<String, Object>(params);
        // Add the required query param 'query' to the map of query params
        allParams.put("query", query);

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
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.PUT, genericUrl, content).execute();
    }


  /**
    * To test \&quot;client\&quot; model
    * To test \&quot;client\&quot; model
    * <p><b>200</b> - successful operation
    * @param body client model
    * @return Client
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public Client testClientModel(Client body) throws IOException {
        HttpResponse response = testClientModelForHttpResponse(body);
        TypeReference<Client> typeRef = new TypeReference<Client>() {};
        return apiClient.getObjectMapper().readValue(response.getContent(), typeRef);
    }

  /**
    * To test \&quot;client\&quot; model
    * To test \&quot;client\&quot; model
    * <p><b>200</b> - successful operation
    * @param body client model
    * @param params Map of query params. A collection will be interpreted as passing in multiple instances of the same query param.
    * @return Client
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public Client testClientModel(Client body, Map<String, Object> params) throws IOException {
        HttpResponse response = testClientModelForHttpResponse(body, params);
        TypeReference<Client> typeRef = new TypeReference<Client>() {};
        return apiClient.getObjectMapper().readValue(response.getContent(), typeRef);
    }

    public HttpResponse testClientModelForHttpResponse(Client body) throws IOException {
        // verify the required parameter 'body' is set
        if (body == null) {
            throw new IllegalArgumentException("Missing the required parameter 'body' when calling testClientModel");
        }
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/fake");

        String localVarUrl = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(localVarUrl);

        HttpContent content = apiClient.new JacksonJsonHttpContent(body);
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.PATCH, genericUrl, content).execute();
    }

      public HttpResponse testClientModelForHttpResponse(java.io.InputStream body, String mediaType) throws IOException {
          // verify the required parameter 'body' is set
              if (body == null) {
              throw new IllegalArgumentException("Missing the required parameter 'body' when calling testClientModel");
              }
              UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/fake");

              String localVarUrl = uriBuilder.build().toString();
              GenericUrl genericUrl = new GenericUrl(localVarUrl);

              HttpContent content = body == null ?
                apiClient.new JacksonJsonHttpContent(null) :
                new InputStreamContent(mediaType == null ? Json.MEDIA_TYPE : mediaType, body);
              return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.PATCH, genericUrl, content).execute();
      }

    public HttpResponse testClientModelForHttpResponse(Client body, Map<String, Object> params) throws IOException {
        // verify the required parameter 'body' is set
        if (body == null) {
            throw new IllegalArgumentException("Missing the required parameter 'body' when calling testClientModel");
        }
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/fake");

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
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.PATCH, genericUrl, content).execute();
    }


  /**
    * Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
    * Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
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
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public void testEndpointParameters(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string, File binary, LocalDate date, OffsetDateTime dateTime, String password, String paramCallback) throws IOException {
        testEndpointParametersForHttpResponse(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, paramCallback);
    }

  /**
    * Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
    * Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
    * <p><b>400</b> - Invalid username supplied
    * <p><b>404</b> - User not found
    * @param number None
    * @param _double None
    * @param patternWithoutDelimiter None
    * @param _byte None
    * @param params Map of query params. A collection will be interpreted as passing in multiple instances of the same query param.
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public void testEndpointParameters(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Map<String, Object> params) throws IOException {
        testEndpointParametersForHttpResponse(number, _double, patternWithoutDelimiter, _byte, params);
    }

    public HttpResponse testEndpointParametersForHttpResponse(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Integer integer, Integer int32, Long int64, Float _float, String string, File binary, LocalDate date, OffsetDateTime dateTime, String password, String paramCallback) throws IOException {
        // verify the required parameter 'number' is set
        if (number == null) {
            throw new IllegalArgumentException("Missing the required parameter 'number' when calling testEndpointParameters");
        }// verify the required parameter '_double' is set
        if (_double == null) {
            throw new IllegalArgumentException("Missing the required parameter '_double' when calling testEndpointParameters");
        }// verify the required parameter 'patternWithoutDelimiter' is set
        if (patternWithoutDelimiter == null) {
            throw new IllegalArgumentException("Missing the required parameter 'patternWithoutDelimiter' when calling testEndpointParameters");
        }// verify the required parameter '_byte' is set
        if (_byte == null) {
            throw new IllegalArgumentException("Missing the required parameter '_byte' when calling testEndpointParameters");
        }
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/fake");

        String localVarUrl = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(localVarUrl);

        HttpContent content = new EmptyContent();
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.POST, genericUrl, content).execute();
    }

    public HttpResponse testEndpointParametersForHttpResponse(BigDecimal number, Double _double, String patternWithoutDelimiter, byte[] _byte, Map<String, Object> params) throws IOException {
        // verify the required parameter 'number' is set
        if (number == null) {
            throw new IllegalArgumentException("Missing the required parameter 'number' when calling testEndpointParameters");
        }// verify the required parameter '_double' is set
        if (_double == null) {
            throw new IllegalArgumentException("Missing the required parameter '_double' when calling testEndpointParameters");
        }// verify the required parameter 'patternWithoutDelimiter' is set
        if (patternWithoutDelimiter == null) {
            throw new IllegalArgumentException("Missing the required parameter 'patternWithoutDelimiter' when calling testEndpointParameters");
        }// verify the required parameter '_byte' is set
        if (_byte == null) {
            throw new IllegalArgumentException("Missing the required parameter '_byte' when calling testEndpointParameters");
        }
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/fake");

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

        HttpContent content = new EmptyContent();
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.POST, genericUrl, content).execute();
    }


  /**
    * To test enum parameters
    * To test enum parameters
    * <p><b>400</b> - Invalid request
    * <p><b>404</b> - Not found
    * @param enumHeaderStringArray Header parameter enum test (string array)
    * @param enumHeaderString Header parameter enum test (string)
    * @param enumQueryStringArray Query parameter enum test (string array)
    * @param enumQueryString Query parameter enum test (string)
    * @param enumQueryInteger Query parameter enum test (double)
    * @param enumQueryDouble Query parameter enum test (double)
    * @param enumFormStringArray Form parameter enum test (string array)
    * @param enumFormString Form parameter enum test (string)
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public void testEnumParameters(List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble, List<String> enumFormStringArray, String enumFormString) throws IOException {
        testEnumParametersForHttpResponse(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumFormStringArray, enumFormString);
    }

  /**
    * To test enum parameters
    * To test enum parameters
    * <p><b>400</b> - Invalid request
    * <p><b>404</b> - Not found
    * @param params Map of query params. A collection will be interpreted as passing in multiple instances of the same query param.
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public void testEnumParameters(Map<String, Object> params) throws IOException {
        testEnumParametersForHttpResponse(params);
    }

    public HttpResponse testEnumParametersForHttpResponse(List<String> enumHeaderStringArray, String enumHeaderString, List<String> enumQueryStringArray, String enumQueryString, Integer enumQueryInteger, Double enumQueryDouble, List<String> enumFormStringArray, String enumFormString) throws IOException {
        
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/fake");
        if (enumQueryStringArray != null) {
            String key = "enum_query_string_array";
            Object value = enumQueryStringArray;
            if (value instanceof Collection) {
                uriBuilder = uriBuilder.queryParam(key, ((Collection) value).toArray());
            } else if (value instanceof Object[]) {
                uriBuilder = uriBuilder.queryParam(key, (Object[]) value);
            } else {
                uriBuilder = uriBuilder.queryParam(key, value);
            }
        }        if (enumQueryString != null) {
            String key = "enum_query_string";
            Object value = enumQueryString;
            if (value instanceof Collection) {
                uriBuilder = uriBuilder.queryParam(key, ((Collection) value).toArray());
            } else if (value instanceof Object[]) {
                uriBuilder = uriBuilder.queryParam(key, (Object[]) value);
            } else {
                uriBuilder = uriBuilder.queryParam(key, value);
            }
        }        if (enumQueryInteger != null) {
            String key = "enum_query_integer";
            Object value = enumQueryInteger;
            if (value instanceof Collection) {
                uriBuilder = uriBuilder.queryParam(key, ((Collection) value).toArray());
            } else if (value instanceof Object[]) {
                uriBuilder = uriBuilder.queryParam(key, (Object[]) value);
            } else {
                uriBuilder = uriBuilder.queryParam(key, value);
            }
        }        if (enumQueryDouble != null) {
            String key = "enum_query_double";
            Object value = enumQueryDouble;
            if (value instanceof Collection) {
                uriBuilder = uriBuilder.queryParam(key, ((Collection) value).toArray());
            } else if (value instanceof Object[]) {
                uriBuilder = uriBuilder.queryParam(key, (Object[]) value);
            } else {
                uriBuilder = uriBuilder.queryParam(key, value);
            }
        }

        String localVarUrl = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(localVarUrl);

        HttpContent content = null;
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.GET, genericUrl, content).execute();
    }

    public HttpResponse testEnumParametersForHttpResponse(Map<String, Object> params) throws IOException {
        
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/fake");

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
    * Fake endpoint to test group parameters (optional)
    * Fake endpoint to test group parameters (optional)
    * <p><b>400</b> - Someting wrong
    * @param requiredStringGroup Required String in group parameters
    * @param requiredBooleanGroup Required Boolean in group parameters
    * @param requiredInt64Group Required Integer in group parameters
    * @param stringGroup String in group parameters
    * @param booleanGroup Boolean in group parameters
    * @param int64Group Integer in group parameters
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public void testGroupParameters(Integer requiredStringGroup, Boolean requiredBooleanGroup, Long requiredInt64Group, Integer stringGroup, Boolean booleanGroup, Long int64Group) throws IOException {
        testGroupParametersForHttpResponse(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, stringGroup, booleanGroup, int64Group);
    }

  /**
    * Fake endpoint to test group parameters (optional)
    * Fake endpoint to test group parameters (optional)
    * <p><b>400</b> - Someting wrong
    * @param requiredStringGroup Required String in group parameters
    * @param requiredBooleanGroup Required Boolean in group parameters
    * @param requiredInt64Group Required Integer in group parameters
    * @param params Map of query params. A collection will be interpreted as passing in multiple instances of the same query param.
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public void testGroupParameters(Integer requiredStringGroup, Boolean requiredBooleanGroup, Long requiredInt64Group, Map<String, Object> params) throws IOException {
        testGroupParametersForHttpResponse(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, params);
    }

    public HttpResponse testGroupParametersForHttpResponse(Integer requiredStringGroup, Boolean requiredBooleanGroup, Long requiredInt64Group, Integer stringGroup, Boolean booleanGroup, Long int64Group) throws IOException {
        // verify the required parameter 'requiredStringGroup' is set
        if (requiredStringGroup == null) {
            throw new IllegalArgumentException("Missing the required parameter 'requiredStringGroup' when calling testGroupParameters");
        }// verify the required parameter 'requiredBooleanGroup' is set
        if (requiredBooleanGroup == null) {
            throw new IllegalArgumentException("Missing the required parameter 'requiredBooleanGroup' when calling testGroupParameters");
        }// verify the required parameter 'requiredInt64Group' is set
        if (requiredInt64Group == null) {
            throw new IllegalArgumentException("Missing the required parameter 'requiredInt64Group' when calling testGroupParameters");
        }
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/fake");
        if (requiredStringGroup != null) {
            String key = "required_string_group";
            Object value = requiredStringGroup;
            if (value instanceof Collection) {
                uriBuilder = uriBuilder.queryParam(key, ((Collection) value).toArray());
            } else if (value instanceof Object[]) {
                uriBuilder = uriBuilder.queryParam(key, (Object[]) value);
            } else {
                uriBuilder = uriBuilder.queryParam(key, value);
            }
        }        if (requiredInt64Group != null) {
            String key = "required_int64_group";
            Object value = requiredInt64Group;
            if (value instanceof Collection) {
                uriBuilder = uriBuilder.queryParam(key, ((Collection) value).toArray());
            } else if (value instanceof Object[]) {
                uriBuilder = uriBuilder.queryParam(key, (Object[]) value);
            } else {
                uriBuilder = uriBuilder.queryParam(key, value);
            }
        }        if (stringGroup != null) {
            String key = "string_group";
            Object value = stringGroup;
            if (value instanceof Collection) {
                uriBuilder = uriBuilder.queryParam(key, ((Collection) value).toArray());
            } else if (value instanceof Object[]) {
                uriBuilder = uriBuilder.queryParam(key, (Object[]) value);
            } else {
                uriBuilder = uriBuilder.queryParam(key, value);
            }
        }        if (int64Group != null) {
            String key = "int64_group";
            Object value = int64Group;
            if (value instanceof Collection) {
                uriBuilder = uriBuilder.queryParam(key, ((Collection) value).toArray());
            } else if (value instanceof Object[]) {
                uriBuilder = uriBuilder.queryParam(key, (Object[]) value);
            } else {
                uriBuilder = uriBuilder.queryParam(key, value);
            }
        }

        String localVarUrl = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(localVarUrl);

        HttpContent content = null;
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.DELETE, genericUrl, content).execute();
    }

    public HttpResponse testGroupParametersForHttpResponse(Integer requiredStringGroup, Boolean requiredBooleanGroup, Long requiredInt64Group, Map<String, Object> params) throws IOException {
        // verify the required parameter 'requiredStringGroup' is set
        if (requiredStringGroup == null) {
            throw new IllegalArgumentException("Missing the required parameter 'requiredStringGroup' when calling testGroupParameters");
        }// verify the required parameter 'requiredBooleanGroup' is set
        if (requiredBooleanGroup == null) {
            throw new IllegalArgumentException("Missing the required parameter 'requiredBooleanGroup' when calling testGroupParameters");
        }// verify the required parameter 'requiredInt64Group' is set
        if (requiredInt64Group == null) {
            throw new IllegalArgumentException("Missing the required parameter 'requiredInt64Group' when calling testGroupParameters");
        }
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/fake");

        // Copy the params argument if present, to allow passing in immutable maps
        Map<String, Object> allParams = params == null ? new HashMap<String, Object>() : new HashMap<String, Object>(params);
        // Add the required query param 'requiredStringGroup' to the map of query params
        allParams.put("requiredStringGroup", requiredStringGroup);
        // Add the required query param 'requiredInt64Group' to the map of query params
        allParams.put("requiredInt64Group", requiredInt64Group);

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
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.DELETE, genericUrl, content).execute();
    }


  /**
    * test inline additionalProperties
    * <p><b>200</b> - successful operation
    * @param param request body
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public void testInlineAdditionalProperties(Map<String, String> param) throws IOException {
        testInlineAdditionalPropertiesForHttpResponse(param);
    }

  /**
    * test inline additionalProperties
    * <p><b>200</b> - successful operation
    * @param param request body
    * @param params Map of query params. A collection will be interpreted as passing in multiple instances of the same query param.
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public void testInlineAdditionalProperties(Map<String, String> param, Map<String, Object> params) throws IOException {
        testInlineAdditionalPropertiesForHttpResponse(param, params);
    }

    public HttpResponse testInlineAdditionalPropertiesForHttpResponse(Map<String, String> param) throws IOException {
        // verify the required parameter 'param' is set
        if (param == null) {
            throw new IllegalArgumentException("Missing the required parameter 'param' when calling testInlineAdditionalProperties");
        }
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/fake/inline-additionalProperties");

        String localVarUrl = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(localVarUrl);

        HttpContent content = apiClient.new JacksonJsonHttpContent(param);
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.POST, genericUrl, content).execute();
    }

      public HttpResponse testInlineAdditionalPropertiesForHttpResponse(java.io.InputStream param, String mediaType) throws IOException {
          // verify the required parameter 'param' is set
              if (param == null) {
              throw new IllegalArgumentException("Missing the required parameter 'param' when calling testInlineAdditionalProperties");
              }
              UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/fake/inline-additionalProperties");

              String localVarUrl = uriBuilder.build().toString();
              GenericUrl genericUrl = new GenericUrl(localVarUrl);

              HttpContent content = param == null ?
                apiClient.new JacksonJsonHttpContent(null) :
                new InputStreamContent(mediaType == null ? Json.MEDIA_TYPE : mediaType, param);
              return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.POST, genericUrl, content).execute();
      }

    public HttpResponse testInlineAdditionalPropertiesForHttpResponse(Map<String, String> param, Map<String, Object> params) throws IOException {
        // verify the required parameter 'param' is set
        if (param == null) {
            throw new IllegalArgumentException("Missing the required parameter 'param' when calling testInlineAdditionalProperties");
        }
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/fake/inline-additionalProperties");

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

        HttpContent content = apiClient.new JacksonJsonHttpContent(param);
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.POST, genericUrl, content).execute();
    }


  /**
    * test json serialization of form data
    * <p><b>200</b> - successful operation
    * @param param field1
    * @param param2 field2
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public void testJsonFormData(String param, String param2) throws IOException {
        testJsonFormDataForHttpResponse(param, param2);
    }

  /**
    * test json serialization of form data
    * <p><b>200</b> - successful operation
    * @param param field1
    * @param param2 field2
    * @param params Map of query params. A collection will be interpreted as passing in multiple instances of the same query param.
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public void testJsonFormData(String param, String param2, Map<String, Object> params) throws IOException {
        testJsonFormDataForHttpResponse(param, param2, params);
    }

    public HttpResponse testJsonFormDataForHttpResponse(String param, String param2) throws IOException {
        // verify the required parameter 'param' is set
        if (param == null) {
            throw new IllegalArgumentException("Missing the required parameter 'param' when calling testJsonFormData");
        }// verify the required parameter 'param2' is set
        if (param2 == null) {
            throw new IllegalArgumentException("Missing the required parameter 'param2' when calling testJsonFormData");
        }
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/fake/jsonFormData");

        String localVarUrl = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(localVarUrl);

        HttpContent content = null;
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.GET, genericUrl, content).execute();
    }

    public HttpResponse testJsonFormDataForHttpResponse(String param, String param2, Map<String, Object> params) throws IOException {
        // verify the required parameter 'param' is set
        if (param == null) {
            throw new IllegalArgumentException("Missing the required parameter 'param' when calling testJsonFormData");
        }// verify the required parameter 'param2' is set
        if (param2 == null) {
            throw new IllegalArgumentException("Missing the required parameter 'param2' when calling testJsonFormData");
        }
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/fake/jsonFormData");

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
    * To test the collection format in query parameters
    * <p><b>200</b> - Success
    * @param pipe The pipe parameter
    * @param ioutil The ioutil parameter
    * @param http The http parameter
    * @param url The url parameter
    * @param context The context parameter
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public void testQueryParameterCollectionFormat(List<String> pipe, List<String> ioutil, List<String> http, List<String> url, List<String> context) throws IOException {
        testQueryParameterCollectionFormatForHttpResponse(pipe, ioutil, http, url, context);
    }

  /**
    * To test the collection format in query parameters
    * <p><b>200</b> - Success
    * @param pipe The pipe parameter
    * @param ioutil The ioutil parameter
    * @param http The http parameter
    * @param url The url parameter
    * @param context The context parameter
    * @param params Map of query params. A collection will be interpreted as passing in multiple instances of the same query param.
    * @throws IOException if an error occurs while attempting to invoke the API
    **/
    public void testQueryParameterCollectionFormat(List<String> pipe, List<String> ioutil, List<String> http, List<String> url, List<String> context, Map<String, Object> params) throws IOException {
        testQueryParameterCollectionFormatForHttpResponse(pipe, ioutil, http, url, context, params);
    }

    public HttpResponse testQueryParameterCollectionFormatForHttpResponse(List<String> pipe, List<String> ioutil, List<String> http, List<String> url, List<String> context) throws IOException {
        // verify the required parameter 'pipe' is set
        if (pipe == null) {
            throw new IllegalArgumentException("Missing the required parameter 'pipe' when calling testQueryParameterCollectionFormat");
        }// verify the required parameter 'ioutil' is set
        if (ioutil == null) {
            throw new IllegalArgumentException("Missing the required parameter 'ioutil' when calling testQueryParameterCollectionFormat");
        }// verify the required parameter 'http' is set
        if (http == null) {
            throw new IllegalArgumentException("Missing the required parameter 'http' when calling testQueryParameterCollectionFormat");
        }// verify the required parameter 'url' is set
        if (url == null) {
            throw new IllegalArgumentException("Missing the required parameter 'url' when calling testQueryParameterCollectionFormat");
        }// verify the required parameter 'context' is set
        if (context == null) {
            throw new IllegalArgumentException("Missing the required parameter 'context' when calling testQueryParameterCollectionFormat");
        }
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/fake/test-query-parameters");
        if (pipe != null) {
            String key = "pipe";
            Object value = pipe;
            if (value instanceof Collection) {
                uriBuilder = uriBuilder.queryParam(key, ((Collection) value).toArray());
            } else if (value instanceof Object[]) {
                uriBuilder = uriBuilder.queryParam(key, (Object[]) value);
            } else {
                uriBuilder = uriBuilder.queryParam(key, value);
            }
        }        if (ioutil != null) {
            String key = "ioutil";
            Object value = ioutil;
            if (value instanceof Collection) {
                uriBuilder = uriBuilder.queryParam(key, ((Collection) value).toArray());
            } else if (value instanceof Object[]) {
                uriBuilder = uriBuilder.queryParam(key, (Object[]) value);
            } else {
                uriBuilder = uriBuilder.queryParam(key, value);
            }
        }        if (http != null) {
            String key = "http";
            Object value = http;
            if (value instanceof Collection) {
                uriBuilder = uriBuilder.queryParam(key, ((Collection) value).toArray());
            } else if (value instanceof Object[]) {
                uriBuilder = uriBuilder.queryParam(key, (Object[]) value);
            } else {
                uriBuilder = uriBuilder.queryParam(key, value);
            }
        }        if (url != null) {
            String key = "url";
            Object value = url;
            if (value instanceof Collection) {
                uriBuilder = uriBuilder.queryParam(key, ((Collection) value).toArray());
            } else if (value instanceof Object[]) {
                uriBuilder = uriBuilder.queryParam(key, (Object[]) value);
            } else {
                uriBuilder = uriBuilder.queryParam(key, value);
            }
        }        if (context != null) {
            String key = "context";
            Object value = context;
            if (value instanceof Collection) {
                uriBuilder = uriBuilder.queryParam(key, ((Collection) value).toArray());
            } else if (value instanceof Object[]) {
                uriBuilder = uriBuilder.queryParam(key, (Object[]) value);
            } else {
                uriBuilder = uriBuilder.queryParam(key, value);
            }
        }

        String localVarUrl = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(localVarUrl);

        HttpContent content = new EmptyContent();
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.PUT, genericUrl, content).execute();
    }

    public HttpResponse testQueryParameterCollectionFormatForHttpResponse(List<String> pipe, List<String> ioutil, List<String> http, List<String> url, List<String> context, Map<String, Object> params) throws IOException {
        // verify the required parameter 'pipe' is set
        if (pipe == null) {
            throw new IllegalArgumentException("Missing the required parameter 'pipe' when calling testQueryParameterCollectionFormat");
        }// verify the required parameter 'ioutil' is set
        if (ioutil == null) {
            throw new IllegalArgumentException("Missing the required parameter 'ioutil' when calling testQueryParameterCollectionFormat");
        }// verify the required parameter 'http' is set
        if (http == null) {
            throw new IllegalArgumentException("Missing the required parameter 'http' when calling testQueryParameterCollectionFormat");
        }// verify the required parameter 'url' is set
        if (url == null) {
            throw new IllegalArgumentException("Missing the required parameter 'url' when calling testQueryParameterCollectionFormat");
        }// verify the required parameter 'context' is set
        if (context == null) {
            throw new IllegalArgumentException("Missing the required parameter 'context' when calling testQueryParameterCollectionFormat");
        }
        UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/fake/test-query-parameters");

        // Copy the params argument if present, to allow passing in immutable maps
        Map<String, Object> allParams = params == null ? new HashMap<String, Object>() : new HashMap<String, Object>(params);
        // Add the required query param 'pipe' to the map of query params
        allParams.put("pipe", pipe);
        // Add the required query param 'ioutil' to the map of query params
        allParams.put("ioutil", ioutil);
        // Add the required query param 'http' to the map of query params
        allParams.put("http", http);
        // Add the required query param 'url' to the map of query params
        allParams.put("url", url);
        // Add the required query param 'context' to the map of query params
        allParams.put("context", context);

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

        HttpContent content = new EmptyContent();
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.PUT, genericUrl, content).execute();
    }


}
