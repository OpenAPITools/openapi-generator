package io.swagger.client;

import android.content.Context;

import com.android.volley.RequestQueue;
import com.android.volley.Response;
import com.android.volley.toolbox.Volley;
import com.google.gson.JsonParseException;

import org.apache.http.Consts;
import org.apache.http.HttpEntity;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;

import java.net.URLEncoder;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import io.swagger.client.auth.Authentication;
import io.swagger.client.auth.ApiKeyAuth;
import io.swagger.client.auth.HttpBasicAuth;
import io.swagger.client.request.GetRequest;
import io.swagger.client.request.PostRequest;
import io.swagger.client.request.PutRequest;
import io.swagger.client.request.DeleteRequest;
import io.swagger.client.request.PatchRequest;

public class ApiInvoker {
  private static ApiInvoker INSTANCE;
  private Map<String, String> defaultHeaderMap = new HashMap<String, String>();

  private Context context;
  private RequestQueue mRequestQueue;

  private Map<String, Authentication> authentications;

  /** Content type "text/plain" with UTF-8 encoding. */
  public static final ContentType TEXT_PLAIN_UTF8 = ContentType.create("text/plain", Consts.UTF_8);

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

  /*
    Format to {@code Pair} objects.
  */
  public static List<Pair> parameterToPairs(String collectionFormat, String name, Object value){
    List<Pair> params = new ArrayList<Pair>();

    // preconditions
    if (name == null || name.isEmpty() || value == null) return params;

    Collection valueCollection = null;
    if (value instanceof Collection) {
      valueCollection = (Collection) value;
    } else {
      params.add(new Pair(name, parameterToString(value)));
      return params;
    }

    if (valueCollection.isEmpty()){
      return params;
    }

    // get the collection format
    collectionFormat = (collectionFormat == null || collectionFormat.isEmpty() ? "csv" : collectionFormat); // default: csv

    // create the params based on the collection format
    if (collectionFormat.equals("multi")) {
      for (Object item : valueCollection) {
        params.add(new Pair(name, parameterToString(item)));
      }

      return params;
    }

    String delimiter = ",";

    if (collectionFormat.equals("csv")) {
      delimiter = ",";
    } else if (collectionFormat.equals("ssv")) {
      delimiter = " ";
    } else if (collectionFormat.equals("tsv")) {
      delimiter = "\t";
    } else if (collectionFormat.equals("pipes")) {
      delimiter = "|";
    }

    StringBuilder sb = new StringBuilder() ;
    for (Object item : valueCollection) {
      sb.append(delimiter);
      sb.append(parameterToString(item));
    }

    params.add(new Pair(name, sb.substring(1)));

    return params;
  }

  public static void initializeInstance(Context context) {
     INSTANCE = new ApiInvoker(context);
     setUserAgent("Android-Volley-Swagger");

     // Setup authentications (key: authentication name, value: authentication).
     INSTANCE.authentications = new HashMap<String, Authentication>();
     
     
     
     
     
     INSTANCE.authentications.put("api_key", new ApiKeyAuth("header", "api_key"));
     
     
     
     // Prevent the authentications from being modified.
     INSTANCE.authentications = Collections.unmodifiableMap(INSTANCE.authentications);
  }
  private ApiInvoker(Context context) {
     this.context = context;
     initConnectionManager();
  }

  public ApiInvoker() {
    initConnectionManager();
  }

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
      if("list".equalsIgnoreCase(containerType) || "array".equalsIgnoreCase(containerType)) {
        return JsonUtil.deserializeToList(json, cls);
      }
      else if(String.class.equals(cls)) {
        if(json != null && json.startsWith("\"") && json.endsWith("\"") && json.length() > 1)
          return json.substring(1, json.length() - 1);
        else
          return json;
      }
      else {
        return JsonUtil.deserializeToObject(json, cls);
      }
    }
    catch (JsonParseException e) {
      throw new ApiException(500, e.getMessage());
    }
  }

  public static String serialize(Object obj) throws ApiException {
    try {
      if (obj != null)
        return JsonUtil.serialize(obj);
      else
        return null;
    }
    catch (Exception e) {
      throw new ApiException(500, e.getMessage());
    }
  }

  /**
    * Get authentications (key: authentication name, value: authentication).
  */
  public Map<String, Authentication> getAuthentications() {
    return authentications;
  }

  /**
  * Get authentication for the given name.
  *
  * @param authName The authentication name
  * @return The authentication, null if not found
  */
  public Authentication getAuthentication(String authName) {
    return authentications.get(authName);
  }

  /**
  * Helper method to set username for the first HTTP basic authentication.
  */
  public void setUsername(String username) {
    for (Authentication auth : authentications.values()) {
       if (auth instanceof HttpBasicAuth) {
           ((HttpBasicAuth) auth).setUsername(username);
           return;
       }
    }
    throw new RuntimeException("No HTTP basic authentication configured!");
  }

  /**
  * Helper method to set password for the first HTTP basic authentication.
  */
  public void setPassword(String password) {
     for (Authentication auth : authentications.values()) {
        if (auth instanceof HttpBasicAuth) {
           ((HttpBasicAuth) auth).setPassword(password);
           return;
        }
     }
     throw new RuntimeException("No HTTP basic authentication configured!");
  }

  /**
  * Helper method to set API key value for the first API key authentication.
  */
  public void setApiKey(String apiKey) {
    for (Authentication auth : authentications.values()) {
      if (auth instanceof ApiKeyAuth) {
        ((ApiKeyAuth) auth).setApiKey(apiKey);
        return;
      }
    }
    throw new RuntimeException("No API key authentication configured!");
  }

  /**
  * Helper method to set API key prefix for the first API key authentication.
  */
  public void setApiKeyPrefix(String apiKeyPrefix) {
    for (Authentication auth : authentications.values()) {
      if (auth instanceof ApiKeyAuth) {
        ((ApiKeyAuth) auth).setApiKeyPrefix(apiKeyPrefix);
        return;
      }
    }
    throw new RuntimeException("No API key authentication configured!");
  }

  /**
  * Update query and header parameters based on authentication settings.
  *
  * @param authNames The authentications to apply
  */
  private void updateParamsForAuth(String[] authNames, List<Pair> queryParams, Map<String, String> headerParams) {
    for (String authName : authNames) {
      Authentication auth = authentications.get(authName);
      if (auth == null) throw new RuntimeException("Authentication undefined: " + authName);
        auth.applyToParams(queryParams, headerParams);
    }
  }

  public void invokeAPI(String host, String path, String method, List<Pair> queryParams, Object body, Map<String, String> headerParams, Map<String, String> formParams, String contentType, String[] authNames, Response.Listener<String> stringRequest, Response.ErrorListener errorListener) throws ApiException {
    StringBuilder b = new StringBuilder();
    b.append("?");

    updateParamsForAuth(authNames, queryParams, headerParams);

    if (queryParams != null){
      for (Pair queryParam : queryParams){
        if (!queryParam.getName().isEmpty()) {
          b.append(escapeString(queryParam.getName()));
          b.append("=");
          b.append(escapeString(queryParam.getValue()));
          b.append("&");
        }
      }
    }

    String querystring = b.substring(0, b.length() - 1);
    String url = host + path + querystring;

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

    // URL encoded string from form parameters
    String formParamStr = null;

    // for form data
    if ("application/x-www-form-urlencoded".equals(contentType)) {
      StringBuilder formParamBuilder = new StringBuilder();

      // encode the form params
      for (String key : formParams.keySet()) {
        String value = formParams.get(key);
        if (value != null && !"".equals(value.trim())) {
          if (formParamBuilder.length() > 0) {
            formParamBuilder.append("&");
          }
          try {
            formParamBuilder.append(URLEncoder.encode(key, "utf8")).append("=").append(URLEncoder.encode(value, "utf8"));
          }
          catch (Exception e) {
            // move on to next
          }
        }
      }
      formParamStr = formParamBuilder.toString();
    }

    if ("GET".equals(method)) {
      GetRequest request = new GetRequest(url, headers, null, stringRequest, errorListener);
      mRequestQueue.add(request);
    }
    else if ("POST".equals(method)) {
       PostRequest request = null;
       if (formParamStr != null) {
          request = new PostRequest(url, headers, contentType, new StringEntity(formParamStr, "UTF-8"), stringRequest, errorListener);
       } else if (body != null) {
          if (body instanceof HttpEntity) {
          request = new PostRequest(url, headers, null, (HttpEntity) body, stringRequest, errorListener);
          } else {
             request = new PostRequest(url, headers, contentType, new StringEntity(serialize(body), "UTF-8"), stringRequest, errorListener);
          }
       }
       if(request != null) mRequestQueue.add(request);
    }
    else if ("PUT".equals(method)) {
       PutRequest request = null;
       if (formParamStr != null) {
          request = new PutRequest(url, headers, contentType, new StringEntity(formParamStr, "UTF-8"), stringRequest, errorListener);
       } else if (body != null) {
          if (body instanceof HttpEntity) {
             request = new PutRequest(url, headers, null, (HttpEntity) body, stringRequest, errorListener);
          } else {
             request = new PutRequest(url, headers, contentType, new StringEntity(serialize(body), "UTF-8"), stringRequest, errorListener);
          }
       }
       if(request != null) mRequestQueue.add(request);
    }
    else if ("DELETE".equals(method)) {
       DeleteRequest request = null;
          if (formParamStr != null) {
           request = new DeleteRequest(url, headers, contentType, new StringEntity(formParamStr, "UTF-8"), stringRequest, errorListener);
          } else if (body != null) {
          if (body instanceof HttpEntity) {
             request = new DeleteRequest(url, headers, null, (HttpEntity) body, stringRequest, errorListener);
          } else {
             request = new DeleteRequest(url, headers, contentType, new StringEntity(serialize(body), "UTF-8"), stringRequest, errorListener);
          }
       }
       if(request != null) mRequestQueue.add(request);
    }
    else if ("PATCH".equals(method)) {
                PatchRequest request = null;
          if (formParamStr != null) {
             request = new PatchRequest(url, headers, contentType, new StringEntity(formParamStr, "UTF-8"), stringRequest, errorListener);
          } else if (body != null) {
             if (body instanceof HttpEntity) {
                request = new PatchRequest(url, headers, null, (HttpEntity) body, stringRequest, errorListener);
             } else {
                request = new PatchRequest(url, headers, contentType, new StringEntity(serialize(body), "UTF-8"), stringRequest, errorListener);
             }
             }
          if(request != null) mRequestQueue.add(request);
       }
  }

  private void initConnectionManager() {
    mRequestQueue = Volley.newRequestQueue(context);
  }
}
