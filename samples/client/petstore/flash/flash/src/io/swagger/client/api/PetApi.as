package io.swagger.client.api {

import io.swagger.common.ApiInvoker;
import io.swagger.exception.ApiErrorCodes;
import io.swagger.exception.ApiError;
import io.swagger.common.ApiUserCredentials;
import io.swagger.event.Response;
import io.swagger.common.SwaggerApi;
import io.swagger.client.model.ApiResponse;
import flash.filesystem.File;
import io.swagger.client.model.Pet;

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

        public static const event_add_pet: String = "add_pet";
        public static const event_delete_pet: String = "delete_pet";
        public static const event_find_pets_by_status: String = "find_pets_by_status";
        public static const event_find_pets_by_tags: String = "find_pets_by_tags";
        public static const event_get_pet_by_id: String = "get_pet_by_id";
        public static const event_update_pet: String = "update_pet";
        public static const event_update_pet_with_form: String = "update_pet_with_form";
        public static const event_upload_file: String = "upload_file";


    /*
     * Returns void 
     */
    public function add_pet (body: Pet): String {
        // create path and map variables
        var path: String = "/pet".replace(/{format}/g,"xml");

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if() {
            throw new ApiError(400, "missing required params");
        }

        
        
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "POST", queryParams, body, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "add_pet";

        token.returnType = null ;
        return requestId;

    }

    /*
     * Returns void 
     */
    public function delete_pet (petId: Number, apiKey: String): String {
        // create path and map variables
        var path: String = "/pet/{petId}".replace(/{format}/g,"xml").replace("{" + "petId" + "}", getApiInvoker().escapeString(petId));

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(        // verify required params are set
        if() {
            throw new ApiError(400, "missing required params");
        }
) {
            throw new ApiError(400, "missing required params");
        }

        
        headerParams["api_key"] = toPathValue(apiKey);

        var token:AsyncToken = getApiInvoker().invokeAPI(path, "DELETE", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "delete_pet";

        token.returnType = null ;
        return requestId;

    }

    /*
     * Returns Array 
     */
    public function find_pets_by_status (status: Array): String {
        // create path and map variables
        var path: String = "/pet/findByStatus".replace(/{format}/g,"xml");

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if() {
            throw new ApiError(400, "missing required params");
        }

        if("null" != String(status))
            queryParams["status"] = toPathValue(status);

        
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "GET", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "find_pets_by_status";

        token.returnType = Array;
        return requestId;

    }

    /*
     * Returns Array 
     */
    public function find_pets_by_tags (tags: Array): String {
        // create path and map variables
        var path: String = "/pet/findByTags".replace(/{format}/g,"xml");

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if() {
            throw new ApiError(400, "missing required params");
        }

        if("null" != String(tags))
            queryParams["tags"] = toPathValue(tags);

        
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "GET", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "find_pets_by_tags";

        token.returnType = Array;
        return requestId;

    }

    /*
     * Returns Pet 
     */
    public function get_pet_by_id (petId: Number): String {
        // create path and map variables
        var path: String = "/pet/{petId}".replace(/{format}/g,"xml").replace("{" + "petId" + "}", getApiInvoker().escapeString(petId));

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if() {
            throw new ApiError(400, "missing required params");
        }

        
        
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "GET", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "get_pet_by_id";

        token.returnType = Pet;
        return requestId;

    }

    /*
     * Returns void 
     */
    public function update_pet (body: Pet): String {
        // create path and map variables
        var path: String = "/pet".replace(/{format}/g,"xml");

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if() {
            throw new ApiError(400, "missing required params");
        }

        
        
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "PUT", queryParams, body, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "update_pet";

        token.returnType = null ;
        return requestId;

    }

    /*
     * Returns void 
     */
    public function update_pet_with_form (petId: Number, name: String, status: String): String {
        // create path and map variables
        var path: String = "/pet/{petId}".replace(/{format}/g,"xml").replace("{" + "petId" + "}", getApiInvoker().escapeString(petId));

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(        // verify required params are set
        if(        // verify required params are set
        if() {
            throw new ApiError(400, "missing required params");
        }
) {
            throw new ApiError(400, "missing required params");
        }
) {
            throw new ApiError(400, "missing required params");
        }

        
        
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "POST", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "update_pet_with_form";

        token.returnType = null ;
        return requestId;

    }

    /*
     * Returns ApiResponse 
     */
    public function upload_file (petId: Number, additionalMetadata: String, file: File): String {
        // create path and map variables
        var path: String = "/pet/{petId}/uploadImage".replace(/{format}/g,"xml").replace("{" + "petId" + "}", getApiInvoker().escapeString(petId));

        // query params
        var queryParams: Dictionary = new Dictionary();
        var headerParams: Dictionary = new Dictionary();

        // verify required params are set
        if(        // verify required params are set
        if(        // verify required params are set
        if() {
            throw new ApiError(400, "missing required params");
        }
) {
            throw new ApiError(400, "missing required params");
        }
) {
            throw new ApiError(400, "missing required params");
        }

        
        
        var token:AsyncToken = getApiInvoker().invokeAPI(path, "POST", queryParams, null, headerParams);

        var requestId: String = getUniqueId();

        token.requestId = requestId;
        token.completionEventType = "upload_file";

        token.returnType = ApiResponse;
        return requestId;

    }
}
}
