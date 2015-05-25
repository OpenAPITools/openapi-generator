package io.swagger.client;

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
import com.sun.jersey.multipart.FormDataMultiPart;

import javax.ws.rs.core.Response.Status.Family;
import javax.ws.rs.core.MediaType;

import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.Map.Entry;
import java.util.HashMap;
import java.util.List;
import java.util.Date;
import java.util.TimeZone;

import java.net.URLEncoder;

import java.io.IOException;
import java.io.UnsupportedEncodingException;

import java.text.SimpleDateFormat;
import java.text.ParseException;

public class ApiInvoker {
  private static ApiInvoker INSTANCE = new ApiInvoker();
  private Map<String, Client> hostMap = new HashMap<String, Client>();
  private Map<String, String> defaultHeaderMap = new HashMap<String, String>();
  private boolean isDebug = false;

  /**
   * ISO 8601 date time format.
   * @see https://en.wikipedia.org/wiki/ISO_8601
   */
  public static final SimpleDateFormat DATE_TIME_FORMAT = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ");

  /**
   * ISO 8601 date format.
   * @see https://en.wikipedia.org/wiki/ISO_8601
   */
  public static final SimpleDateFormat DATE_FORMAT = new SimpleDateFormat("yyyy-MM-dd");

  static {
    // Use UTC as the default time zone.
    DATE_TIME_FORMAT.setTimeZone(TimeZone.getTimeZone("UTC"));
    DATE_FORMAT.setTimeZone(TimeZone.getTimeZone("UTC"));

    // Set default User-Agent.
    setUserAgent("Java-Swagger");
  }

  public static void setUserAgent(String userAgent) {
    INSTANCE.addDefaultHeader("User-Agent", userAgent);
  }

  public static Date parseDateTime(String str) {
    try {
      return DATE_TIME_FORMAT.parse(str);
    } catch (java.text.ParseException e) {
      throw new RuntimeException(e);
    }
  }

  public static Date parseDate(String str) {
    try {
      return DATE_FORMAT.parse(str);
    } catch (java.text.ParseException e) {
      throw new RuntimeException(e);
    }
  }

  public static String formatDateTime(Date datetime) {
    return DATE_TIME_FORMAT.format(datetime);
  }

  public static String formatDate(Date date) {
    return DATE_FORMAT.format(date);
  }

  public static String parameterToString(Object param) {
    if (param == null) {
      return "";
    } else if (param instanceof Date) {
      return formatDateTime((Date) param);
    } else if (param instanceof Collection) {
      StringBuilder b = new StringBuilder();
      for(Object o : (Collection)param) {
        if(b.length() > 0) {
          b.append(",");
        }
        b.append(String.valueOf(o));
      }
      return b.toString();
    } else {
      return String.valueOf(param);
    }
  }

  public static String selectHeaderAccept(String[] accepts) {
    if (accepts.length == 0) return "application/json";
    if (StringUtil.containsIgnoreCase(accepts, "application/json")) return "application/json";
    return StringUtil.join(accepts, ",");
  }

  public static String selectHeaderContentType(String[] contentTypes) {
    if (contentTypes.length == 0) return "application/json";
    if (StringUtil.containsIgnoreCase(contentTypes, "application/json")) return "application/json";
    return contentTypes[0];
  }

  public void enableDebug() {
    isDebug = true;
  }

  public static ApiInvoker getInstance() {
    return INSTANCE;
  }

  public void addDefaultHeader(String key, String value) {
     defaultHeaderMap.put(key, value);
  }

  public String escapeString(String str) {
    try{
      return URLEncoder.encode(str, "utf8").replaceAll("\\+", "%20");
    }
    catch(UnsupportedEncodingException e) {
      return str;
    }
  }

  public static Object deserialize(String json, String containerType, Class cls) throws ApiException {
    if(null != containerType) {
        containerType = containerType.toLowerCase();
    }
    try{
      if("list".equals(containerType) || "array".equals(containerType)) {
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

  public String invokeAPI(String host, String path, String method, Map<String, String> queryParams, Object body, Map<String, String> headerParams, Map<String, String> formParams, String accept, String contentType) throws ApiException {
    Client client = getClient(host);

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
    String querystring = b.toString();

    Builder builder = client.resource(host + path + querystring).accept(accept);
    for(String key : headerParams.keySet()) {
      builder = builder.header(key, headerParams.get(key));
    }

    for(String key : defaultHeaderMap.keySet()) {
      if(!headerParams.containsKey(key)) {
        builder = builder.header(key, defaultHeaderMap.get(key));
      }
    }
    ClientResponse response = null;

    if("GET".equals(method)) {
      response = (ClientResponse) builder.get(ClientResponse.class);
    }
    else if ("POST".equals(method)) {
      if (contentType.startsWith("application/x-www-form-urlencoded")) {
        String encodedFormParams = this
            .getXWWWFormUrlencodedParams(formParams);
        response = builder.type(contentType).post(ClientResponse.class,
            encodedFormParams);
      } else if (body == null) {
        response = builder.post(ClientResponse.class, null);
      } else if(body instanceof FormDataMultiPart) {
        response = builder.type(contentType).post(ClientResponse.class, body);
      }
      else
        response = builder.type(contentType).post(ClientResponse.class, serialize(body));
    }
    else if ("PUT".equals(method)) {
      if ("application/x-www-form-urlencoded".equals(contentType)) {
          String encodedFormParams = this
              .getXWWWFormUrlencodedParams(formParams);
          response = builder.type(contentType).put(ClientResponse.class,
              encodedFormParams);
      } else if(body == null) {
        response = builder.put(ClientResponse.class, serialize(body));
      } else {
          response = builder.type(contentType).put(ClientResponse.class, serialize(body));
      }
    }
    else if ("DELETE".equals(method)) {
      if ("application/x-www-form-urlencoded".equals(contentType)) {
        String encodedFormParams = this
            .getXWWWFormUrlencodedParams(formParams);
        response = builder.type(contentType).delete(ClientResponse.class,
            encodedFormParams);
      } else if(body == null) {
        response = builder.delete(ClientResponse.class);
      } else {
        response = builder.type(contentType).delete(ClientResponse.class, serialize(body));
      }
    }
    else {
      throw new ApiException(500, "unknown method type " + method);
    }
    if(response.getClientResponseStatus() == ClientResponse.Status.NO_CONTENT) {
      return null;
    }
    else if(response.getClientResponseStatus().getFamily() == Family.SUCCESSFUL) {
      if(response.hasEntity()) {
        return (String) response.getEntity(String.class);
      }
      else {
        return "";
      }
    }
    else {
      String message = "error";
      if(response.hasEntity()) {
        try{
          message = String.valueOf(response.getEntity(String.class));
        }
        catch (RuntimeException e) {
          // e.printStackTrace();
        }
      }
      throw new ApiException(
                response.getClientResponseStatus().getStatusCode(),
                message);
    }
  }

  private String getXWWWFormUrlencodedParams(Map<String, String> formParams) {
    StringBuilder formParamBuilder = new StringBuilder();

    for (Entry<String, String> param : formParams.entrySet()) {
      String keyStr = ApiInvoker.parameterToString(param.getKey());
      String valueStr = ApiInvoker.parameterToString(param.getValue());

      try {
        formParamBuilder.append(URLEncoder.encode(keyStr, "utf8"))
            .append("=")
            .append(URLEncoder.encode(valueStr, "utf8"));
        formParamBuilder.append("&");
      } catch (UnsupportedEncodingException e) {
        // move on to next
      }
    }
    String encodedFormParams = formParamBuilder.toString();
    if (encodedFormParams.endsWith("&")) {
      encodedFormParams = encodedFormParams.substring(0,
          encodedFormParams.length() - 1);
    }
    return encodedFormParams;
  }


  private Client getClient(String host) {
    if(!hostMap.containsKey(host)) {
      Client client = Client.create();
      if(isDebug)
        client.addFilter(new LoggingFilter());
      hostMap.put(host, client);
    }
    return hostMap.get(host);
  }
}
