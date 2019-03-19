package org.openapitools.api;

import groovyx.net.http.*
import static groovyx.net.http.ContentType.*
import static groovyx.net.http.Method.*
import org.openapitools.api.ApiUtils

import org.openapitools.model.ModelApiResponse
import org.openapitools.model.Pet

import java.util.*;

@Mixin(ApiUtils)
class PetApi {
    String basePath = "http://petstore.swagger.io/v2"
    String versionPath = "/api/v1"

    def addPet ( Pet body, Closure onSuccess, Closure onFailure)  {
        // create path and map path parameters (TODO)
        String resourcePath = "/pet"

        // query params
        def queryParams = [:]
        def headerParams = [:]

        // verify required params are set
        if (body == null) {
            throw new RuntimeException("missing required params body")
        }

        // TODO: form params, body param not yet support

        invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams,
                    "POST", "",
                    null )

    }

    def deletePet ( Long petId, String apiKey, Closure onSuccess, Closure onFailure)  {
        // create path and map path parameters (TODO)
        String resourcePath = "/pet/{petId}"

        // query params
        def queryParams = [:]
        def headerParams = [:]

        // verify required params are set
        if (petId == null) {
            throw new RuntimeException("missing required params petId")
        }

        headerParams.put("api_key", apiKey)

        // TODO: form params, body param not yet support

        invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams,
                    "DELETE", "",
                    null )

    }

    def findPetsByStatus ( List<String> status, Closure onSuccess, Closure onFailure)  {
        // create path and map path parameters (TODO)
        String resourcePath = "/pet/findByStatus"

        // query params
        def queryParams = [:]
        def headerParams = [:]

        // verify required params are set
        if (status == null) {
            throw new RuntimeException("missing required params status")
        }

        if (!"null".equals(String.valueOf(status)))
            queryParams.put("status", String.valueOf(status))

        // TODO: form params, body param not yet support

        invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams,
                    "GET", "array",
                    Pet.class )

    }

    def findPetsByTags ( List<String> tags, Closure onSuccess, Closure onFailure)  {
        // create path and map path parameters (TODO)
        String resourcePath = "/pet/findByTags"

        // query params
        def queryParams = [:]
        def headerParams = [:]

        // verify required params are set
        if (tags == null) {
            throw new RuntimeException("missing required params tags")
        }

        if (!"null".equals(String.valueOf(tags)))
            queryParams.put("tags", String.valueOf(tags))

        // TODO: form params, body param not yet support

        invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams,
                    "GET", "array",
                    Pet.class )

    }

    def getPetById ( Long petId, Closure onSuccess, Closure onFailure)  {
        // create path and map path parameters (TODO)
        String resourcePath = "/pet/{petId}"

        // query params
        def queryParams = [:]
        def headerParams = [:]

        // verify required params are set
        if (petId == null) {
            throw new RuntimeException("missing required params petId")
        }

        // TODO: form params, body param not yet support

        invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams,
                    "GET", "",
                    Pet.class )

    }

    def updatePet ( Pet body, Closure onSuccess, Closure onFailure)  {
        // create path and map path parameters (TODO)
        String resourcePath = "/pet"

        // query params
        def queryParams = [:]
        def headerParams = [:]

        // verify required params are set
        if (body == null) {
            throw new RuntimeException("missing required params body")
        }

        // TODO: form params, body param not yet support

        invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams,
                    "PUT", "",
                    null )

    }

    def updatePetWithForm ( Long petId, String name, String status, Closure onSuccess, Closure onFailure)  {
        // create path and map path parameters (TODO)
        String resourcePath = "/pet/{petId}"

        // query params
        def queryParams = [:]
        def headerParams = [:]

        // verify required params are set
        if (petId == null) {
            throw new RuntimeException("missing required params petId")
        }

        // TODO: form params, body param not yet support

        invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams,
                    "POST", "",
                    null )

    }

    def uploadFile ( Long petId, String additionalMetadata, File file, Closure onSuccess, Closure onFailure)  {
        // create path and map path parameters (TODO)
        String resourcePath = "/pet/{petId}/uploadImage"

        // query params
        def queryParams = [:]
        def headerParams = [:]

        // verify required params are set
        if (petId == null) {
            throw new RuntimeException("missing required params petId")
        }

        // TODO: form params, body param not yet support

        invokeApi(onSuccess, onFailure, basePath, versionPath, resourcePath, queryParams, headerParams,
                    "POST", "",
                    ModelApiResponse.class )

    }

}
