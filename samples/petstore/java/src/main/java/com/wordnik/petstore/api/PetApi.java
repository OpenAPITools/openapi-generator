package com.wordnik.petstore.api;

import com.wordnik.client.ApiException;
import com.wordnik.client.ApiInvoker;
import com.wordnik.petstore.model.Pet;
import java.util.*;

public class PetApi {
  String basePath = "http://petstore.swagger.wordnik.com/api";
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

  public Pet getPetById (String petId) throws ApiException {
    // create path and map variables
    String path = "/pet.{format}/{petId}".replaceAll("\\{format\\}","json").replaceAll("\\{" + "petId" + "\\}", apiInvoker.escapeString(petId));

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();

    // verify required params are set
    if(petId == null ) {
       throw new ApiException(400, "missing required params");
    }
    try {
      String response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, null, headerParams);
      if(response != null){
        return (Pet) ApiInvoker.deserialize(response, "", Pet.class);
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
  public void addPet (Pet body) throws ApiException {
    // create path and map variables
    String path = "/pet.{format}".replaceAll("\\{format\\}","json");

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();

    // verify required params are set
    if(body == null ) {
       throw new ApiException(400, "missing required params");
    }
    try {
      String response = apiInvoker.invokeAPI(basePath, path, "POST", queryParams, body, headerParams);
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
  public void updatePet (Pet body) throws ApiException {
    // create path and map variables
    String path = "/pet.{format}".replaceAll("\\{format\\}","json");

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();

    // verify required params are set
    if(body == null ) {
       throw new ApiException(400, "missing required params");
    }
    try {
      String response = apiInvoker.invokeAPI(basePath, path, "PUT", queryParams, body, headerParams);
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
  public List<Pet> findPetsByStatus (String status) throws ApiException {
    // create path and map variables
    String path = "/pet.{format}/findByStatus".replaceAll("\\{format\\}","json");

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();

    // verify required params are set
    if(status == null ) {
       throw new ApiException(400, "missing required params");
    }
    if(!"null".equals(String.valueOf(status)))
      queryParams.put("status", String.valueOf(status));
    try {
      String response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, null, headerParams);
      if(response != null){
        return (List<Pet>) ApiInvoker.deserialize(response, "List", Pet.class);
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
  public List<Pet> findPetsByTags (String tags) throws ApiException {
    // create path and map variables
    String path = "/pet.{format}/findByTags".replaceAll("\\{format\\}","json");

    // query params
    Map<String, String> queryParams = new HashMap<String, String>();
    Map<String, String> headerParams = new HashMap<String, String>();

    // verify required params are set
    if(tags == null ) {
       throw new ApiException(400, "missing required params");
    }
    if(!"null".equals(String.valueOf(tags)))
      queryParams.put("tags", String.valueOf(tags));
    try {
      String response = apiInvoker.invokeAPI(basePath, path, "GET", queryParams, null, headerParams);
      if(response != null){
        return (List<Pet>) ApiInvoker.deserialize(response, "List", Pet.class);
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

