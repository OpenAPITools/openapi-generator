package com.wordnik.petstore.api;

import com.wordnik.client.ApiException;
import com.wordnik.client.ApiInvoker;
import com.wordnik.petstore.model.User;
import java.util.*;
import java.io.File;

import org.apache.http.NameValuePair;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.entity.mime.*;
import org.apache.http.entity.mime.content.*;
import org.apache.http.entity.ContentType;

import android.webkit.MimeTypeMap;

public class UserApi {
  String basePath = "http://petstore.swagger.wordnik.com/api";
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

  private static String getMimeType(File file) {
    MimeTypeMap mime = MimeTypeMap.getSingleton();
    int index = file.getName().lastIndexOf('.')+1;
    String ext = file.getName().substring(index).toLowerCase();
    return mime.getMimeTypeFromExtension(ext);
  }

  public void createUsersWithArrayInput (List<User> body) throws ApiException {
    Object postBody = body;
    // verify required params are set
    if(body == null ) {
       throw new ApiException(400, "missing required params");
    }
    // create path and map variables
    String path = "/user/createWithArray".replaceAll("\\{format\\}","json");

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();

    String[] contentTypes = {
      "application/json"};

    String contentType = contentTypes.length > 0 ? contentTypes[0] : "application/json";

    if(contentType.startsWith("application/x-www-form-urlencoded")) {
      boolean hasFields = false;
      List<NameValuePair> mp = new ArrayList<NameValuePair>();
      if(hasFields)
        postBody = mp;
    }
    else if(contentType.startsWith("multipart/form-data")) {
      boolean hasFields = false;
      MultipartEntityBuilder builder = MultipartEntityBuilder.create();
      builder.setMode(HttpMultipartMode.BROWSER_COMPATIBLE);
      if(hasFields)
        postBody = builder;
    }
    else {
      postBody = body;
    }

    try {
      String response = apiInvoker.invokeAPI(basePath, path, "POST", queryParams, postBody, headerParams, contentType);
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
  public void createUsersWithListInput (List<User> body) throws ApiException {
    Object postBody = body;
    // verify required params are set
    if(body == null ) {
       throw new ApiException(400, "missing required params");
    }
    // create path and map variables
    String path = "/user/createWithList".replaceAll("\\{format\\}","json");

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();

    String[] contentTypes = {
      "application/json"};

    String contentType = contentTypes.length > 0 ? contentTypes[0] : "application/json";

    if(contentType.startsWith("application/x-www-form-urlencoded")) {
      boolean hasFields = false;
      List<NameValuePair> mp = new ArrayList<NameValuePair>();
      if(hasFields)
        postBody = mp;
    }
    else if(contentType.startsWith("multipart/form-data")) {
      boolean hasFields = false;
      MultipartEntityBuilder builder = MultipartEntityBuilder.create();
      builder.setMode(HttpMultipartMode.BROWSER_COMPATIBLE);
      if(hasFields)
        postBody = builder;
    }
    else {
      postBody = body;
    }

    try {
      String response = apiInvoker.invokeAPI(basePath, path, "POST", queryParams, postBody, headerParams, contentType);
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
  //error info- code: 400 reason: "Invalid username supplied" model: <none>
  //error info- code: 404 reason: "User not found" model: <none>
  public void updateUser (String username, User body) throws ApiException {
    Object postBody = body;
    // verify required params are set
    if(username == null || body == null ) {
       throw new ApiException(400, "missing required params");
    }
    // create path and map variables
    String path = "/user/{username}".replaceAll("\\{format\\}","json").replaceAll("\\{" + "username" + "\\}", apiInvoker.escapeString(username.toString()));

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();

    String[] contentTypes = {
      "application/json"};

    String contentType = contentTypes.length > 0 ? contentTypes[0] : "application/json";

    if(contentType.startsWith("application/x-www-form-urlencoded")) {
      boolean hasFields = false;
      List<NameValuePair> mp = new ArrayList<NameValuePair>();
      if(hasFields)
        postBody = mp;
    }
    else if(contentType.startsWith("multipart/form-data")) {
      boolean hasFields = false;
      MultipartEntityBuilder builder = MultipartEntityBuilder.create();
      builder.setMode(HttpMultipartMode.BROWSER_COMPATIBLE);
      if(hasFields)
        postBody = builder;
    }
    else {
      postBody = body;
    }

    try {
      String response = apiInvoker.invokeAPI(basePath, path, "PUT", queryParams, postBody, headerParams, contentType);
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
  //error info- code: 400 reason: "Invalid username supplied" model: <none>
  //error info- code: 404 reason: "User not found" model: <none>
  public void deleteUser (String username) throws ApiException {
    Object postBody = null;
    // verify required params are set
    if(username == null ) {
       throw new ApiException(400, "missing required params");
    }
    // create path and map variables
    String path = "/user/{username}".replaceAll("\\{format\\}","json").replaceAll("\\{" + "username" + "\\}", apiInvoker.escapeString(username.toString()));

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();

    String[] contentTypes = {
      "application/json"};

    String contentType = contentTypes.length > 0 ? contentTypes[0] : "application/json";

    if(contentType.startsWith("application/x-www-form-urlencoded")) {
      boolean hasFields = false;
      List<NameValuePair> mp = new ArrayList<NameValuePair>();
      if(hasFields)
        postBody = mp;
    }
    else if(contentType.startsWith("multipart/form-data")) {
      boolean hasFields = false;
      MultipartEntityBuilder builder = MultipartEntityBuilder.create();
      builder.setMode(HttpMultipartMode.BROWSER_COMPATIBLE);
      if(hasFields)
        postBody = builder;
    }
    else {
      postBody = null;
    }

    try {
      String response = apiInvoker.invokeAPI(basePath, path, "DELETE", queryParams, postBody, headerParams, contentType);
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
  //error info- code: 400 reason: "Invalid username supplied" model: <none>
  //error info- code: 404 reason: "User not found" model: <none>
  public User getUserByName (String username) throws ApiException {
    Object postBody = null;
    // verify required params are set
    if(username == null ) {
       throw new ApiException(400, "missing required params");
    }
    // create path and map variables
    String path = "/user/{username}".replaceAll("\\{format\\}","json").replaceAll("\\{" + "username" + "\\}", apiInvoker.escapeString(username.toString()));

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();

    String[] contentTypes = {
      "application/json"};

    String contentType = contentTypes.length > 0 ? contentTypes[0] : "application/json";

    if(contentType.startsWith("application/x-www-form-urlencoded")) {
      boolean hasFields = false;
      List<NameValuePair> mp = new ArrayList<NameValuePair>();
      if(hasFields)
        postBody = mp;
    }
    else if(contentType.startsWith("multipart/form-data")) {
      boolean hasFields = false;
      MultipartEntityBuilder builder = MultipartEntityBuilder.create();
      builder.setMode(HttpMultipartMode.BROWSER_COMPATIBLE);
      if(hasFields)
        postBody = builder;
    }
    else {
      postBody = null;
    }

    try {
      String response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, postBody, headerParams, contentType);
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
  //error info- code: 400 reason: "Invalid username and password combination" model: <none>
  public String loginUser (String username, String password) throws ApiException {
    Object postBody = null;
    // verify required params are set
    if(username == null || password == null ) {
       throw new ApiException(400, "missing required params");
    }
    // create path and map variables
    String path = "/user/login".replaceAll("\\{format\\}","json");

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();

    if(!"null".equals(String.valueOf(username)))
      queryParams.put("username", String.valueOf(username));
    if(!"null".equals(String.valueOf(password)))
      queryParams.put("password", String.valueOf(password));
    String[] contentTypes = {
      "application/json"};

    String contentType = contentTypes.length > 0 ? contentTypes[0] : "application/json";

    if(contentType.startsWith("application/x-www-form-urlencoded")) {
      boolean hasFields = false;
      List<NameValuePair> mp = new ArrayList<NameValuePair>();
      if(hasFields)
        postBody = mp;
    }
    else if(contentType.startsWith("multipart/form-data")) {
      boolean hasFields = false;
      MultipartEntityBuilder builder = MultipartEntityBuilder.create();
      builder.setMode(HttpMultipartMode.BROWSER_COMPATIBLE);
      if(hasFields)
        postBody = builder;
    }
    else {
      postBody = null;
    }

    try {
      String response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, postBody, headerParams, contentType);
      if(response != null){
        return (String) ApiInvoker.deserialize(response, "", String.class);
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
  public void logoutUser () throws ApiException {
    Object postBody = null;
    // create path and map variables
    String path = "/user/logout".replaceAll("\\{format\\}","json");

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();

    String[] contentTypes = {
      "application/json"};

    String contentType = contentTypes.length > 0 ? contentTypes[0] : "application/json";

    if(contentType.startsWith("application/x-www-form-urlencoded")) {
      boolean hasFields = false;
      List<NameValuePair> mp = new ArrayList<NameValuePair>();
      if(hasFields)
        postBody = mp;
    }
    else if(contentType.startsWith("multipart/form-data")) {
      boolean hasFields = false;
      MultipartEntityBuilder builder = MultipartEntityBuilder.create();
      builder.setMode(HttpMultipartMode.BROWSER_COMPATIBLE);
      if(hasFields)
        postBody = builder;
    }
    else {
      postBody = null;
    }

    try {
      String response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, postBody, headerParams, contentType);
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
  public void createUser (User body) throws ApiException {
    Object postBody = body;
    // verify required params are set
    if(body == null ) {
       throw new ApiException(400, "missing required params");
    }
    // create path and map variables
    String path = "/user".replaceAll("\\{format\\}","json");

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();

    String[] contentTypes = {
      "application/json"};

    String contentType = contentTypes.length > 0 ? contentTypes[0] : "application/json";

    if(contentType.startsWith("application/x-www-form-urlencoded")) {
      boolean hasFields = false;
      List<NameValuePair> mp = new ArrayList<NameValuePair>();
      if(hasFields)
        postBody = mp;
    }
    else if(contentType.startsWith("multipart/form-data")) {
      boolean hasFields = false;
      MultipartEntityBuilder builder = MultipartEntityBuilder.create();
      builder.setMode(HttpMultipartMode.BROWSER_COMPATIBLE);
      if(hasFields)
        postBody = builder;
    }
    else {
      postBody = body;
    }

    try {
      String response = apiInvoker.invokeAPI(basePath, path, "POST", queryParams, postBody, headerParams, contentType);
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

