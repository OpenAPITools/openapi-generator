package com.wordnik.client.api {

import com.wordnik.swagger.common.ApiInvoker;
import com.wordnik.swagger.exception.ApiErrorCodes;
import com.wordnik.swagger.exception.ApiError;
import com.wordnik.swagger.common.ApiUserCredentials;
import com.wordnik.swagger.event.Response;
import com.wordnik.swagger.common.SwaggerApi;
import com.wordnik.client.model.Pet;
import com.wordnik.client.model.File;
import mx.rpc.AsyncToken;
import mx.utils.UIDUtil;
import flash.utils.Dictionary;
import flash.events.EventDispatcher;

public class PetApi extends SwaggerApi {
    /**
    * Constructor for the PetApi api client
    * @param apiCredentials Wrapper object for tokens and hostName required towards authentication
    * @param eventDispatcher Optional event dispatcher that when provided is used by the SDK to dispatch any Response
    */
    public function PetApi(apiCredentials: ApiUserCredentials, eventDispatcher: EventDispatcher = null) {
        super(apiCredentials, eventDispatcher);
    }

public static const event_getPetById: String = "getPetById";
public static const event_deletePet: String = "deletePet";
public static const event_partialUpdate: String = "partialUpdate";
public static const event_updatePetWithForm: String = "updatePetWithForm";
public static const event_uploadFile: String = "uploadFile";
public static const event_addPet: String = "addPet";
public static const event_updatePet: String = "updatePet";
public static const event_findPetsByStatus: String = "findPetsByStatus";
public static const event_findPetsByTags: String = "findPetsByTags";
/*
     * Returns Pet */
    public function getPetById (petId: Number): String {
        // create path and map variables
        var path: String = "/pet/{petId}".replace(/{format}/g,"xml").replace("{" + "petId" + "}", getApiInvoker().escapeString(petId));

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(petId == null ) {
            throw new ApiError(400, "missing required params");
        }
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "GET", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "getPetById";

        token.returnType = Pet;
        return requestId;

    }
    /*
     * Returns void */
    public function deletePet (petId: String): String {
        // create path and map variables
        var path: String = "/pet/{petId}".replace(/{format}/g,"xml").replace("{" + "petId" + "}", getApiInvoker().escapeString(petId));

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(petId == null ) {
            throw new ApiError(400, "missing required params");
        }
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "DELETE", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "deletePet";

        token.returnType = null ;
        return requestId;

    }
    /*
     * Returns Array[Pet] */
    public function partialUpdate (petId: String, body: Pet): String {
        // create path and map variables
        var path: String = "/pet/{petId}".replace(/{format}/g,"xml").replace("{" + "petId" + "}", getApiInvoker().escapeString(petId));

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(petId == null || body == null ) {
            throw new ApiError(400, "missing required params");
        }
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "PATCH", queryParams, body, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "partialUpdate";

        token.returnType = Array[Pet];
        return requestId;

    }
    /*
     * Returns void */
    public function updatePetWithForm (petId: String, name: String, status: String): String {
        // create path and map variables
        var path: String = "/pet/{petId}".replace(/{format}/g,"xml").replace("{" + "petId" + "}", getApiInvoker().escapeString(petId));

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(petId == null ) {
            throw new ApiError(400, "missing required params");
        }
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "POST", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "updatePetWithForm";

        token.returnType = null ;
        return requestId;

    }
    /*
     * Returns void */
    public function uploadFile (additionalMetadata: String, body: File): String {
        // create path and map variables
        var path: String = "/pet/uploadImage".replace(/{format}/g,"xml");

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        var token:AsyncToken = getApiInvoker().invokeAPI(path, "POST", queryParams, body, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "uploadFile";

        token.returnType = null ;
        return requestId;

    }
    /*
     * Returns void */
    public function addPet (body: Pet): String {
        // create path and map variables
        var path: String = "/pet".replace(/{format}/g,"xml");

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(body == null ) {
            throw new ApiError(400, "missing required params");
        }
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "POST", queryParams, body, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "addPet";

        token.returnType = null ;
        return requestId;

    }
    /*
     * Returns void */
    public function updatePet (body: Pet): String {
        // create path and map variables
        var path: String = "/pet".replace(/{format}/g,"xml");

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(body == null ) {
            throw new ApiError(400, "missing required params");
        }
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "PUT", queryParams, body, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "updatePet";

        token.returnType = null ;
        return requestId;

    }
    /*
     * Returns Array[Pet] */
    public function findPetsByStatus (status: String= "available"): String {
        // create path and map variables
        var path: String = "/pet/findByStatus".replace(/{format}/g,"xml");

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(status == null ) {
            throw new ApiError(400, "missing required params");
        }
        if("null" != String(status))
            queryParams["status"] = toPathValue(status);
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "GET", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "findPetsByStatus";

        token.returnType = Array[Pet];
        return requestId;

    }
    /*
     * Returns Array[Pet] */
    public function findPetsByTags (tags: String): String {
        // create path and map variables
        var path: String = "/pet/findByTags".replace(/{format}/g,"xml");

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(tags == null ) {
            throw new ApiError(400, "missing required params");
        }
        if("null" != String(tags))
            queryParams["tags"] = toPathValue(tags);
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "GET", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "findPetsByTags";

        token.returnType = Array[Pet];
        return requestId;

    }
    }
        }
