package io.swagger.api;





import groovyx.net.http.*
import static groovyx.net.http.ContentType.*
import static groovyx.net.http.Method.*
import io.swagger.api.ApiUtils
//-------------

import io.swagger.model.Pet
import java.io.File
import io.swagger.model.ModelApiResponse

import java.util.*;

@Mixin(ApiUtils)
class PetApi {
    String basePath = "http://petstore.swagger.io/v2"
    String versionPath = "/api/v1"


  def addPet ( Pet body, Closure onSuccess, Closure onFailure)  {
    // create path and map variables
    String resourcePath = ""


    // query params
    def queryParams = [:]
    def headerParams = [:]

    // verify required params are set
    if() {
       throw new RuntimeException("missing required params")
    }

    
    
    invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams,
                    "POST", "",
                    null )

  }
  def deletePet ( Long petId, String apiKey, Closure onSuccess, Closure onFailure)  {
    // create path and map variables
    String resourcePath = "/{petId}"


    // query params
    def queryParams = [:]
    def headerParams = [:]

    // verify required params are set
    if(    // verify required params are set
    if() {
       throw new RuntimeException("missing required params")
    }
) {
       throw new RuntimeException("missing required params")
    }

    
    headerParams.put("apiKey", apiKey)

    invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams,
                    "DELETE", "",
                    null )

  }
  def findPetsByStatus ( List<String> status, Closure onSuccess, Closure onFailure)  {
    // create path and map variables
    String resourcePath = "/findByStatus"


    // query params
    def queryParams = [:]
    def headerParams = [:]

    // verify required params are set
    if() {
       throw new RuntimeException("missing required params")
    }

    if(!"null".equals(String.valueOf(status)))
      queryParams.put("status", String.valueOf(status))

    
    invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams,
                    "GET", "List",
                    Pet.class )

  }
  def findPetsByTags ( List<String> tags, Closure onSuccess, Closure onFailure)  {
    // create path and map variables
    String resourcePath = "/findByTags"


    // query params
    def queryParams = [:]
    def headerParams = [:]

    // verify required params are set
    if() {
       throw new RuntimeException("missing required params")
    }

    if(!"null".equals(String.valueOf(tags)))
      queryParams.put("tags", String.valueOf(tags))

    
    invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams,
                    "GET", "List",
                    Pet.class )

  }
  def getPetById ( Long petId, Closure onSuccess, Closure onFailure)  {
    // create path and map variables
    String resourcePath = "/{petId}"


    // query params
    def queryParams = [:]
    def headerParams = [:]

    // verify required params are set
    if() {
       throw new RuntimeException("missing required params")
    }

    
    
    invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams,
                    "GET", "",
                    Pet.class )

  }
  def updatePet ( Pet body, Closure onSuccess, Closure onFailure)  {
    // create path and map variables
    String resourcePath = ""


    // query params
    def queryParams = [:]
    def headerParams = [:]

    // verify required params are set
    if() {
       throw new RuntimeException("missing required params")
    }

    
    
    invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams,
                    "PUT", "",
                    null )

  }
  def updatePetWithForm ( Long petId, String name, String status, Closure onSuccess, Closure onFailure)  {
    // create path and map variables
    String resourcePath = "/{petId}"


    // query params
    def queryParams = [:]
    def headerParams = [:]

    // verify required params are set
    if(    // verify required params are set
    if(    // verify required params are set
    if() {
       throw new RuntimeException("missing required params")
    }
) {
       throw new RuntimeException("missing required params")
    }
) {
       throw new RuntimeException("missing required params")
    }

    
    
    invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams,
                    "POST", "",
                    null )

  }
  def uploadFile ( Long petId, String additionalMetadata, File file, Closure onSuccess, Closure onFailure)  {
    // create path and map variables
    String resourcePath = "/{petId}/uploadImage"


    // query params
    def queryParams = [:]
    def headerParams = [:]

    // verify required params are set
    if(    // verify required params are set
    if(    // verify required params are set
    if() {
       throw new RuntimeException("missing required params")
    }
) {
       throw new RuntimeException("missing required params")
    }
) {
       throw new RuntimeException("missing required params")
    }

    
    
    invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams,
                    "POST", "",
                    ModelApiResponse.class )

  }
}
