package com.wordnik.client.api {

import com.wordnik.swagger.common.ApiInvoker;
import com.wordnik.swagger.exception.ApiErrorCodes;
import com.wordnik.swagger.exception.ApiError;
import com.wordnik.swagger.common.ApiUserCredentials;
import com.wordnik.swagger.event.Response;
import com.wordnik.swagger.common.SwaggerApi;
import com.wordnik.client.model.Pet;
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
public static const event_addPet: String = "addPet";
public static const event_updatePet: String = "updatePet";
public static const event_findPetsByStatus: String = "findPetsByStatus";
public static const event_findPetsByTags: String = "findPetsByTags";
/*
     * Returns Pet */
    public function getPetById (petId: String): String {
        // create path and map variables
        var path: String = "/pet.{format}/{petId}".replace(/{format}/g,"xml").replace("{" + "petId" + "}", getApiInvoker().escapeString(petId));

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
    public function addPet (body: Pet): String {
        // create path and map variables
        var path: String = "/pet.{format}".replace(/{format}/g,"xml");

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
        var path: String = "/pet.{format}".replace(/{format}/g,"xml");

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
     * Returns com.wordnik.client.model.PetList */
    public function findPetsByStatus (status: String= "available"): String {
        // create path and map variables
        var path: String = "/pet.{format}/findByStatus".replace(/{format}/g,"xml");

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

        token.returnType = com.wordnik.client.model.PetList;
        return requestId;

    }
    /*
     * Returns com.wordnik.client.model.PetList */
    public function findPetsByTags (tags: String): String {
        // create path and map variables
        var path: String = "/pet.{format}/findByTags".replace(/{format}/g,"xml");

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

        token.returnType = com.wordnik.client.model.PetList;
        return requestId;

    }
    }
        }
