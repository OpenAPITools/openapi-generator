package com.wordnik.client;

import com.wordnik.swagger.core.util.JsonUtil;

import com.fasterxml.jackson.core.JsonGenerator.Feature;
import com.fasterxml.jackson.databind.*;
import com.fasterxml.jackson.annotation.*;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;

import com.sun.jersey.api.client.Client;
import com.sun.jersey.api.client.ClientResponse;
import com.sun.jersey.api.client.config.ClientConfig;
import com.sun.jersey.api.client.config.DefaultClientConfig;
import com.sun.jersey.api.client.filter.LoggingFilter;
import com.sun.jersey.api.client.WebResource.Builder;

import javax.ws.rs.core.MediaType;

import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.io.IOException;

public class ApiInvoker {
  private static ApiInvoker INSTANCE = new ApiInvoker();
  private Map<String, Client> hostMap = new HashMap<String, Client>();
  private Map<String, String> defaultHeaderMap = new HashMap<String, String>();

  public static ApiInvoker getInstance() {
    return INSTANCE;
  }
  
  public void addDefaultHeader(String key, String value) {
     defaultHeaderMap.put(key, value);
  }

  public String escapeString(String str) {
    return str;
  }

  public static Object deserialize(String json, String containerType, Class cls) throws ApiException {
    try{
      if("List".equals(containerType)) {
        JavaType typeInfo = JsonUtil.getJsonMapper().getTypeFactory().constructCollectionType(List.class, cls);
        List response = (List<?>) JsonUtil.getJsonMapper().readValue(json, typeInfo);
        return response;
      }
      else if(String.class.equals(cls)) {
        if(json != null && json.startsWith("\"") && json.endsWith("\"") && json.length() > 1) {
          return json.substring(1, json.length() - 2);
        }
        else 
          return json;
      }
      else {
        return JsonUtil.getJsonMapper().readValue(json, cls);
      }
    }
    catch (IOException e) {
      throw new ApiException(500, e.getMessage());
    }
  }

  public static String serialize(Object obj) throws ApiException {
    try {
      if (obj != null) return JsonUtil.getJsonMapper().writeValueAsString(obj);
      else return null;
    }
    catch (Exception e) {
      throw new ApiException(500, e.getMessage());
    }
  }

  public String invokeAPI(String host, String path, String method, Map<String, String> queryParams, Object body, Map<String, String> headerParams) throws ApiException {
    Client client = getClient(host);

    StringBuilder b = new StringBuilder();
    
    for(String key : queryParams.keySet()) {
      String value = queryParams.get(key);
      if (value != null){
        if(b.toString().length() == 0) b.append("?");
        else b.append("&");
        b.append(escapeString(key)).append("=").append(escapeString(value));
      }
    }
    String querystring = b.toString();

    Builder builder = client.resource(host + path + querystring).type("application/json");
    for(String key : headerParams.keySet()) {
      builder.header(key, headerParams.get(key));
    }
    
    for(String key : defaultHeaderMap.keySet()) {
      if(!headerParams.containsKey(key)) {
        builder.header(key, defaultHeaderMap.get(key));
      }
    }
    ClientResponse response = null;

    if("GET".equals(method)) {
      response = (ClientResponse) builder.get(ClientResponse.class);
    }
    else if ("POST".equals(method)) {
        response = builder.post(ClientResponse.class, serialize(body));
    }
    else if ("PUT".equals(method)) {
      response = builder.put(ClientResponse.class, serialize(body));
      }
    else if ("DELETE".equals(method)) {
        response = builder.delete(ClientResponse.class, serialize(body));
    }
    else {
      throw new ApiException(500, "unknown method type " + method);
    }
    if(response.getClientResponseStatus() == ClientResponse.Status.OK) {
      return (String) response.getEntity(String.class);
    }
    else {
      throw new ApiException(
                response.getClientResponseStatus().getStatusCode(),
                response.getEntity(String.class));      
    }
  }

  private Client getClient(String host) {
  if(!hostMap.containsKey(host)) {
    Client client = Client.create();
    client.addFilter(new LoggingFilter());
        hostMap.put(host, client);
  }
  return hostMap.get(host);
  }
}

