package org.openapitools.client.api;

import org.openapitools.client.ApiClient;

import org.openapitools.client.model.XmlItem;

import com.fasterxml.jackson.core.type.TypeReference;
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

        String url = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(url);

        HttpContent content = apiClient.new JacksonJsonHttpContent(xmlItem);
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.POST, genericUrl, content).execute();
    }

      public HttpResponse createXmlItemForHttpResponse(java.io.InputStream xmlItem, String mediaType) throws IOException {
          // verify the required parameter 'xmlItem' is set
              if (xmlItem == null) {
              throw new IllegalArgumentException("Missing the required parameter 'xmlItem' when calling createXmlItem");
              }
              UriBuilder uriBuilder = UriBuilder.fromUri(apiClient.getBasePath() + "/fake/create_xml_item");

              String url = uriBuilder.build().toString();
              GenericUrl genericUrl = new GenericUrl(url);

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

        String url = uriBuilder.build().toString();
        GenericUrl genericUrl = new GenericUrl(url);

        HttpContent content = apiClient.new JacksonJsonHttpContent(xmlItem);
        return apiClient.getHttpRequestFactory().buildRequest(HttpMethods.POST, genericUrl, content).execute();
    }


}
