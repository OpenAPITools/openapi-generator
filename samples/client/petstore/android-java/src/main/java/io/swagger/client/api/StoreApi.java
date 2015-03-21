package io.swagger.client.api;

import io.swagger.client.ApiException;
import io.swagger.client.ApiInvoker;

import io.swagger.client.model.*;

import java.util.*;

import java.util.Map;
import io.swagger.client.model.Order;

import org.apache.http.HttpEntity;
import org.apache.http.entity.mime.MultipartEntityBuilder;

import java.util.Map;
import java.util.HashMap;
import java.io.File;

public class StoreApi {
  String basePath = "http://petstore.swagger.io/v2";
  ApiInvoker apiInvoker = ApiInvoker.getInstance();

  public void addHeader(String key, String value) {
    getInvoker().addDefaultHeader(key, value);
  }

  public ApiInvoker getInvoker() {
    return apiInvoker;
  }

  public void setBasePath(String basePath) {
    this.basePath = basePath;
  }

  public String getBasePath() {
    return basePath;
  }

  
  
  public Map<String, Integer>  getInventory () throws ApiException {
    Object postBody = null;

    

    // create path and map variables
    String path = "/store/inventory".replaceAll("\\{format\\}","json");

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    // header params
    Map<String, String> headerParams = new HashMap<String, String>();
    // form params
    Map<String, String> formParams = new HashMap<String, String>();

    

    

    String[] contentTypes = {
      
    };
    String contentType = contentTypes.length > 0 ? contentTypes[0] : "application/json";

    if (contentType.startsWith("multipart/form-data")) {
      // file uploading
      MultipartEntityBuilder builder = MultipartEntityBuilder.create();
      

      HttpEntity httpEntity = builder.build();
      postBody = httpEntity;
    } else {
      // normal form params
      
    }

    try {
      String response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, postBody, headerParams, formParams, contentType);
      if(response != null){
        return (Map<String, Integer>) ApiInvoker.deserialize(response, "map", Map.class);
      }
      else {
        return null;
      }
    } catch (ApiException ex) {
      if(ex.getCode() == 404) {
        return  null;
      }
      else {
        throw ex;
      }
    }
  }
  
  
  public Order  placeOrder (Order body) throws ApiException {
    Object postBody = body;

    

    // create path and map variables
    String path = "/store/order".replaceAll("\\{format\\}","json");

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    // header params
    Map<String, String> headerParams = new HashMap<String, String>();
    // form params
    Map<String, String> formParams = new HashMap<String, String>();

    

    

    String[] contentTypes = {
      
    };
    String contentType = contentTypes.length > 0 ? contentTypes[0] : "application/json";

    if (contentType.startsWith("multipart/form-data")) {
      // file uploading
      MultipartEntityBuilder builder = MultipartEntityBuilder.create();
      

      HttpEntity httpEntity = builder.build();
      postBody = httpEntity;
    } else {
      // normal form params
      
    }

    try {
      String response = apiInvoker.invokeAPI(basePath, path, "POST", queryParams, postBody, headerParams, formParams, contentType);
      if(response != null){
        return (Order) ApiInvoker.deserialize(response, "", Order.class);
      }
      else {
        return null;
      }
    } catch (ApiException ex) {
      if(ex.getCode() == 404) {
        return  null;
      }
      else {
        throw ex;
      }
    }
  }
  
  
  public Order  getOrderById (String orderId) throws ApiException {
    Object postBody = null;

    

    // create path and map variables
    String path = "/store/order/{orderId}".replaceAll("\\{format\\}","json").replaceAll("\\{" + "orderId" + "\\}", apiInvoker.escapeString(orderId.toString()));

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    // header params
    Map<String, String> headerParams = new HashMap<String, String>();
    // form params
    Map<String, String> formParams = new HashMap<String, String>();

    

    

    String[] contentTypes = {
      
    };
    String contentType = contentTypes.length > 0 ? contentTypes[0] : "application/json";

    if (contentType.startsWith("multipart/form-data")) {
      // file uploading
      MultipartEntityBuilder builder = MultipartEntityBuilder.create();
      

      HttpEntity httpEntity = builder.build();
      postBody = httpEntity;
    } else {
      // normal form params
      
    }

    try {
      String response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, postBody, headerParams, formParams, contentType);
      if(response != null){
        return (Order) ApiInvoker.deserialize(response, "", Order.class);
      }
      else {
        return null;
      }
    } catch (ApiException ex) {
      if(ex.getCode() == 404) {
        return  null;
      }
      else {
        throw ex;
      }
    }
  }
  
  
  public void  deleteOrder (String orderId) throws ApiException {
    Object postBody = null;

    

    // create path and map variables
    String path = "/store/order/{orderId}".replaceAll("\\{format\\}","json").replaceAll("\\{" + "orderId" + "\\}", apiInvoker.escapeString(orderId.toString()));

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    // header params
    Map<String, String> headerParams = new HashMap<String, String>();
    // form params
    Map<String, String> formParams = new HashMap<String, String>();

    

    

    String[] contentTypes = {
      
    };
    String contentType = contentTypes.length > 0 ? contentTypes[0] : "application/json";

    if (contentType.startsWith("multipart/form-data")) {
      // file uploading
      MultipartEntityBuilder builder = MultipartEntityBuilder.create();
      

      HttpEntity httpEntity = builder.build();
      postBody = httpEntity;
    } else {
      // normal form params
      
    }

    try {
      String response = apiInvoker.invokeAPI(basePath, path, "DELETE", queryParams, postBody, headerParams, formParams, contentType);
      if(response != null){
        return ;
      }
      else {
        return ;
      }
    } catch (ApiException ex) {
      if(ex.getCode() == 404) {
        return ;
      }
      else {
        throw ex;
      }
    }
  }
  
}
