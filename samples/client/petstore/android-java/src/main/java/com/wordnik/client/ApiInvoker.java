package com.wordnik.client;


import com.fasterxml.jackson.core.JsonGenerator.Feature;
import com.fasterxml.jackson.databind.*;
import com.fasterxml.jackson.annotation.*;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;

import org.apache.http.*;
import org.apache.http.client.*;
import org.apache.http.client.methods.*;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.*;
import org.apache.http.util.EntityUtils;

import java.io.File;
import java.net.URLEncoder;

import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.io.IOException;
import java.io.UnsupportedEncodingException;

public class ApiInvoker {
  private static ApiInvoker INSTANCE = new ApiInvoker();
  private Map<String, String> defaultHeaderMap = new HashMap<String, String>();

  private HttpClient client = null;
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
        if(json != null && json.startsWith("\"") && json.endsWith("\"") && json.length() > 1)
          return json.substring(1, json.length() - 2);
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
      if (obj != null) 
        return JsonUtil.getJsonMapper().writeValueAsString(obj);
      else 
        return null;
    }
    catch (Exception e) {
      throw new ApiException(500, e.getMessage());
    }
  }

  public String invokeAPI(String host, String path, String method, Map<String, String> queryParams, Object body, Map<String, String> headerParams, String contentType) throws ApiException {
    HttpClient client = getClient(host);

    StringBuilder b = new StringBuilder();
    for(String key : queryParams.keySet()) {
      String value = queryParams.get(key);
      if (value != null){
        if(b.toString().length() == 0)
          b.append("?");
        else
          b.append("&");
        b.append(escapeString(key)).append("=").append(escapeString(value));
      }
    }
    String url = host + path + b.toString();

    HashMap<String, String> headers = new HashMap<String, String>();

    for(String key : headerParams.keySet()) {
      headers.put(key, headerParams.get(key));
    }
    
    for(String key : defaultHeaderMap.keySet()) {
      if(!headerParams.containsKey(key)) {
        headers.put(key, defaultHeaderMap.get(key));
      }
    }
    headers.put("Accept", "application/json");

    HttpResponse response = null;
    try{
      if("GET".equals(method)) {
        HttpGet get = new HttpGet(url);
        get.addHeader("Accept", "application/json");
        for(String key : headers.keySet()) {
          get.setHeader(key, headers.get(key));
        }
        response = client.execute(get);
      }
      else if ("POST".equals(method)) {
        HttpPost post = new HttpPost(url);
        post.setHeader("Content-Type", contentType);
        post.setEntity(new StringEntity(serialize(body), "UTF-8"));
        for(String key : headers.keySet()) {
          post.setHeader(key, headers.get(key));
        }
        response = client.execute(post);
      }
      else if ("PUT".equals(method)) {
        HttpPut put = new HttpPut(url);
        if(body != null) {
          put.setHeader("Content-Type", contentType);
          put.setEntity(new StringEntity(serialize(body), "UTF-8"));
        }
        for(String key : headers.keySet()) {
          put.setHeader(key, headers.get(key));
        }
        response = client.execute(put);
      }
      else if ("DELETE".equals(method)) {
        HttpDelete delete = new HttpDelete(url);
        for(String key : headers.keySet()) {
          delete.setHeader(key, headers.get(key));
        }
        response = client.execute(delete);
      }

      int code = response.getStatusLine().getStatusCode();
      String responseString = null;
      if(code == 204) 
        responseString = "";
      else if(code >= 200 && code < 300) {
        if(response.getEntity() != null) {
          HttpEntity resEntity = response.getEntity();
          responseString = EntityUtils.toString(resEntity);
        }
      }
      else {
        if(response.getEntity() != null) {
          HttpEntity resEntity = response.getEntity();
          responseString = EntityUtils.toString(resEntity);
        }
        else
          responseString = "no data";
        throw new ApiException(code, responseString);
      }
      return responseString;
    }
    catch(IOException e) {
      throw new ApiException(500, e.getMessage());
    }
  }

  private HttpClient getClient(String host) {
    if(client == null)
      client = new DefaultHttpClient();
    return client;
  }
}
