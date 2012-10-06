package com.wordnik.client.api;

import com.wordnik.client.common.ApiException;
import com.wordnik.client.common.ApiInvoker;
import com.wordnik.client.model.ApiTokenStatus;
import com.wordnik.client.model.WordList;
import com.wordnik.client.model.User;
import com.wordnik.client.model.AuthenticationToken;
import java.util.*;

public class AccountApi {
  String basePath = "http://api.wordnik.com/v4";
  ApiInvoker apiInvoker = ApiInvoker.getInstance();

  public ApiInvoker getInvoker() {
    return apiInvoker;
  }
  
  public void setBasePath(String basePath) {
    this.basePath = basePath;
  }
  
  public String getBasePath() {
    return basePath;
  }

  public AuthenticationToken authenticate (String username, String password) throws ApiException {
    // create path and map variables
    String path = "/account.{format}/authenticate/{username}".replaceAll("\\{format\\}","json").replaceAll("\\{" + "username" + "\\}", apiInvoker.escapeString(username));

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();

    // verify required params are set
    if(username == null || password == null ) {
       throw new ApiException(400, "missing required params");
    }
    if(!"null".equals(String.valueOf(password)))
      queryParams.put("password", String.valueOf(password));
    try {
      String response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, null, headerParams);
      if(response != null){
        return (AuthenticationToken) ApiInvoker.deserialize(response, "", AuthenticationToken.class);
      }
      else {
        return null;
      }
    } catch (ApiException ex) {
      if(ex.getCode() == 404) {
      	return null;
      }
      else {
        throw ex;
      }
    }
  }
  public AuthenticationToken authenticatePost (String username, String body) throws ApiException {
    // create path and map variables
    String path = "/account.{format}/authenticate/{username}".replaceAll("\\{format\\}","json").replaceAll("\\{" + "username" + "\\}", apiInvoker.escapeString(username));

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();

    // verify required params are set
    if(username == null || body == null ) {
       throw new ApiException(400, "missing required params");
    }
    try {
      String response = apiInvoker.invokeAPI(basePath, path, "POST", queryParams, body, headerParams);
      if(response != null){
        return (AuthenticationToken) ApiInvoker.deserialize(response, "", AuthenticationToken.class);
      }
      else {
        return null;
      }
    } catch (ApiException ex) {
      if(ex.getCode() == 404) {
      	return null;
      }
      else {
        throw ex;
      }
    }
  }
  public List<WordList> getWordListsForLoggedInUser (String auth_token, Integer skip, Integer limit) throws ApiException {
    // create path and map variables
    String path = "/account.{format}/wordLists".replaceAll("\\{format\\}","json");

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();

    // verify required params are set
    if(auth_token == null ) {
       throw new ApiException(400, "missing required params");
    }
    if(!"null".equals(String.valueOf(skip)))
      queryParams.put("skip", String.valueOf(skip));
    if(!"null".equals(String.valueOf(limit)))
      queryParams.put("limit", String.valueOf(limit));
    headerParams.put("auth_token", auth_token);
    try {
      String response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, null, headerParams);
      if(response != null){
        return (List<WordList>) ApiInvoker.deserialize(response, "List", WordList.class);
      }
      else {
        return null;
      }
    } catch (ApiException ex) {
      if(ex.getCode() == 404) {
      	return null;
      }
      else {
        throw ex;
      }
    }
  }
  public ApiTokenStatus getApiTokenStatus (String api_key) throws ApiException {
    // create path and map variables
    String path = "/account.{format}/apiTokenStatus".replaceAll("\\{format\\}","json");

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();

    headerParams.put("api_key", api_key);
    try {
      String response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, null, headerParams);
      if(response != null){
        return (ApiTokenStatus) ApiInvoker.deserialize(response, "", ApiTokenStatus.class);
      }
      else {
        return null;
      }
    } catch (ApiException ex) {
      if(ex.getCode() == 404) {
      	return null;
      }
      else {
        throw ex;
      }
    }
  }
  public User getLoggedInUser (String auth_token) throws ApiException {
    // create path and map variables
    String path = "/account.{format}/user".replaceAll("\\{format\\}","json");

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();

    // verify required params are set
    if(auth_token == null ) {
       throw new ApiException(400, "missing required params");
    }
    headerParams.put("auth_token", auth_token);
    try {
      String response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, null, headerParams);
      if(response != null){
        return (User) ApiInvoker.deserialize(response, "", User.class);
      }
      else {
        return null;
      }
    } catch (ApiException ex) {
      if(ex.getCode() == 404) {
      	return null;
      }
      else {
        throw ex;
      }
    }
  }
  }

