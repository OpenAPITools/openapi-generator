// TODO: better import syntax?
import { BaseAPIRequestFactory, RequiredError } from './baseapi';
import {Configuration} from '../configuration';
import { RequestContext, HttpMethod, ResponseContext, HttpFile} from '../http/http';
import * as FormData from "form-data";
import {ObjectSerializer} from '../models/ObjectSerializer';
import {ApiException} from './exception';
import {isCodeInRange} from '../util';

import { ApiResponse } from '../models/ApiResponse';
import { Pet } from '../models/Pet';

/**
 * no description
 */
export class PetApiRequestFactory extends BaseAPIRequestFactory {
	
    /**
     * Add a new pet to the store
     * @param body Pet object that needs to be added to the store
     */
    public addPet(body: Pet, options?: Configuration): RequestContext {
		let config = options || this.configuration;
		
        // verify required parameter 'body' is not null or undefined
        if (body === null || body === undefined) {
            throw new RequiredError('Required parameter body was null or undefined when calling addPet.');
        }

		
		// Path Params
    	const localVarPath = '/pet';

		// Make Request Context
    	const requestContext = config.baseServer.makeRequestContext(localVarPath, HttpMethod.POST);
        requestContext.setHeaderParam("Accept", "application/json")

        // Query Params
	
		// Header Params
	
		// Form Params


		// Body Params
        requestContext.setHeaderParam("Content-Type", "application/json");
		// TODO: Should this be handled by ObjectSerializer? imo yes => confidential information included in local object should not be sent
        const needsSerialization = (<any>"Pet" !== "string") || requestContext.getHeaders()['Content-Type'] === 'application/json';
        const serializedBody = needsSerialization ? JSON.stringify(body || {}) : (body.toString() || ""); // TODO: `toString` call is unnecessary
        requestContext.setBody(serializedBody);
		
		let authMethod = null;
    	// Apply auth methods
    	authMethod = config.authMethods["petstore_auth"]
    	if (authMethod) {
    		authMethod.applySecurityAuthentication(requestContext);
    	}
    	
    	return requestContext;
    }
			
    /**
     * Deletes a pet
     * @param petId Pet id to delete
     * @param apiKey 
     */
    public deletePet(petId: number, apiKey?: string, options?: Configuration): RequestContext {
		let config = options || this.configuration;
		
        // verify required parameter 'petId' is not null or undefined
        if (petId === null || petId === undefined) {
            throw new RequiredError('Required parameter petId was null or undefined when calling deletePet.');
        }

		
		
		// Path Params
    	const localVarPath = '/pet/{petId}'
            .replace('{' + 'petId' + '}', encodeURIComponent(String(petId)));

		// Make Request Context
    	const requestContext = config.baseServer.makeRequestContext(localVarPath, HttpMethod.DELETE);
        requestContext.setHeaderParam("Accept", "application/json")

        // Query Params
	
		// Header Params
		requestContext.setHeaderParam("api_key", ObjectSerializer.serialize(apiKey, "string", ""));
	
		// Form Params


		// Body Params
		
		let authMethod = null;
    	// Apply auth methods
    	authMethod = config.authMethods["petstore_auth"]
    	if (authMethod) {
    		authMethod.applySecurityAuthentication(requestContext);
    	}
    	
    	return requestContext;
    }
			
    /**
     * Multiple status values can be provided with comma separated strings
     * Finds Pets by status
     * @param status Status values that need to be considered for filter
     */
    public findPetsByStatus(status: Array<'available' | 'pending' | 'sold'>, options?: Configuration): RequestContext {
		let config = options || this.configuration;
		
        // verify required parameter 'status' is not null or undefined
        if (status === null || status === undefined) {
            throw new RequiredError('Required parameter status was null or undefined when calling findPetsByStatus.');
        }

		
		// Path Params
    	const localVarPath = '/pet/findByStatus';

		// Make Request Context
    	const requestContext = config.baseServer.makeRequestContext(localVarPath, HttpMethod.GET);
        requestContext.setHeaderParam("Accept", "application/json")

        // Query Params
        if (status !== undefined) {
        	requestContext.setQueryParam("status", ObjectSerializer.serialize(status, "Array<'available' | 'pending' | 'sold'>", ""));
        }
	
		// Header Params
	
		// Form Params


		// Body Params
		
		let authMethod = null;
    	// Apply auth methods
    	authMethod = config.authMethods["petstore_auth"]
    	if (authMethod) {
    		authMethod.applySecurityAuthentication(requestContext);
    	}
    	
    	return requestContext;
    }
			
    /**
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     * Finds Pets by tags
     * @param tags Tags to filter by
     */
    public findPetsByTags(tags: Array<string>, options?: Configuration): RequestContext {
		let config = options || this.configuration;
		
        // verify required parameter 'tags' is not null or undefined
        if (tags === null || tags === undefined) {
            throw new RequiredError('Required parameter tags was null or undefined when calling findPetsByTags.');
        }

		
		// Path Params
    	const localVarPath = '/pet/findByTags';

		// Make Request Context
    	const requestContext = config.baseServer.makeRequestContext(localVarPath, HttpMethod.GET);
        requestContext.setHeaderParam("Accept", "application/json")

        // Query Params
        if (tags !== undefined) {
        	requestContext.setQueryParam("tags", ObjectSerializer.serialize(tags, "Array<string>", ""));
        }
	
		// Header Params
	
		// Form Params


		// Body Params
		
		let authMethod = null;
    	// Apply auth methods
    	authMethod = config.authMethods["petstore_auth"]
    	if (authMethod) {
    		authMethod.applySecurityAuthentication(requestContext);
    	}
    	
    	return requestContext;
    }
			
    /**
     * Returns a single pet
     * Find pet by ID
     * @param petId ID of pet to return
     */
    public getPetById(petId: number, options?: Configuration): RequestContext {
		let config = options || this.configuration;
		
        // verify required parameter 'petId' is not null or undefined
        if (petId === null || petId === undefined) {
            throw new RequiredError('Required parameter petId was null or undefined when calling getPetById.');
        }

		
		// Path Params
    	const localVarPath = '/pet/{petId}'
            .replace('{' + 'petId' + '}', encodeURIComponent(String(petId)));

		// Make Request Context
    	const requestContext = config.baseServer.makeRequestContext(localVarPath, HttpMethod.GET);
        requestContext.setHeaderParam("Accept", "application/json")

        // Query Params
	
		// Header Params
	
		// Form Params


		// Body Params
		
		let authMethod = null;
    	// Apply auth methods
    	authMethod = config.authMethods["api_key"]
    	if (authMethod) {
    		authMethod.applySecurityAuthentication(requestContext);
    	}
    	
    	return requestContext;
    }
			
    /**
     * Update an existing pet
     * @param body Pet object that needs to be added to the store
     */
    public updatePet(body: Pet, options?: Configuration): RequestContext {
		let config = options || this.configuration;
		
        // verify required parameter 'body' is not null or undefined
        if (body === null || body === undefined) {
            throw new RequiredError('Required parameter body was null or undefined when calling updatePet.');
        }

		
		// Path Params
    	const localVarPath = '/pet';

		// Make Request Context
    	const requestContext = config.baseServer.makeRequestContext(localVarPath, HttpMethod.PUT);
        requestContext.setHeaderParam("Accept", "application/json")

        // Query Params
	
		// Header Params
	
		// Form Params


		// Body Params
        requestContext.setHeaderParam("Content-Type", "application/json");
		// TODO: Should this be handled by ObjectSerializer? imo yes => confidential information included in local object should not be sent
        const needsSerialization = (<any>"Pet" !== "string") || requestContext.getHeaders()['Content-Type'] === 'application/json';
        const serializedBody = needsSerialization ? JSON.stringify(body || {}) : (body.toString() || ""); // TODO: `toString` call is unnecessary
        requestContext.setBody(serializedBody);
		
		let authMethod = null;
    	// Apply auth methods
    	authMethod = config.authMethods["petstore_auth"]
    	if (authMethod) {
    		authMethod.applySecurityAuthentication(requestContext);
    	}
    	
    	return requestContext;
    }
			
    /**
     * Updates a pet in the store with form data
     * @param petId ID of pet that needs to be updated
     * @param name Updated name of the pet
     * @param status Updated status of the pet
     */
    public updatePetWithForm(petId: number, name?: string, status?: string, options?: Configuration): RequestContext {
		let config = options || this.configuration;
		
        // verify required parameter 'petId' is not null or undefined
        if (petId === null || petId === undefined) {
            throw new RequiredError('Required parameter petId was null or undefined when calling updatePetWithForm.');
        }

		
		
		
		// Path Params
    	const localVarPath = '/pet/{petId}'
            .replace('{' + 'petId' + '}', encodeURIComponent(String(petId)));

		// Make Request Context
    	const requestContext = config.baseServer.makeRequestContext(localVarPath, HttpMethod.POST);
        requestContext.setHeaderParam("Accept", "application/json")

        // Query Params
	
		// Header Params
	
		// Form Params
		let localVarFormParams = new FormData();

        if (name !== undefined) {
        // TODO: replace .append with .set
             localVarFormParams.append('name', name as any);
        }
        if (status !== undefined) {
        // TODO: replace .append with .set
             localVarFormParams.append('status', status as any);
        }
		requestContext.setBody(localVarFormParams);

		// Body Params
		
		let authMethod = null;
    	// Apply auth methods
    	authMethod = config.authMethods["petstore_auth"]
    	if (authMethod) {
    		authMethod.applySecurityAuthentication(requestContext);
    	}
    	
    	return requestContext;
    }
			
    /**
     * uploads an image
     * @param petId ID of pet to update
     * @param additionalMetadata Additional data to pass to server
     * @param file file to upload
     */
    public uploadFile(petId: number, additionalMetadata?: string, file?: HttpFile, options?: Configuration): RequestContext {
		let config = options || this.configuration;
		
        // verify required parameter 'petId' is not null or undefined
        if (petId === null || petId === undefined) {
            throw new RequiredError('Required parameter petId was null or undefined when calling uploadFile.');
        }

		
		
		
		// Path Params
    	const localVarPath = '/pet/{petId}/uploadImage'
            .replace('{' + 'petId' + '}', encodeURIComponent(String(petId)));

		// Make Request Context
    	const requestContext = config.baseServer.makeRequestContext(localVarPath, HttpMethod.POST);
        requestContext.setHeaderParam("Accept", "application/json")

        // Query Params
	
		// Header Params
	
		// Form Params
		let localVarFormParams = new FormData();

        if (additionalMetadata !== undefined) {
        // TODO: replace .append with .set
             localVarFormParams.append('additionalMetadata', additionalMetadata as any);
        }
        if (file !== undefined) {
        // TODO: replace .append with .set
             localVarFormParams.append('file', file.data, file.name);
        }
		requestContext.setBody(localVarFormParams);

		// Body Params
		
		let authMethod = null;
    	// Apply auth methods
    	authMethod = config.authMethods["petstore_auth"]
    	if (authMethod) {
    		authMethod.applySecurityAuthentication(requestContext);
    	}
    	
    	return requestContext;
    }
			
}



export class PetApiResponseProcessor {
	
	/**
	 * Unwraps the actual response sent by the server from the response context and deserializes the response content 
	 * to the expected objects
	 * 
	 * @params response Response returned by the server for a request to  
	 * @throws ApiException if the response code was not in [200, 299]
	 */
    public addPet(response: ResponseContext):   void  {      
        if (isCodeInRange("405", response.httpStatusCode)) {
            throw new ApiException<string>(response.httpStatusCode, "Invalid input");
        }
        
        // Work around for incorrect api specification in petstore.yaml
        if (response.httpStatusCode >= 200 && response.httpStatusCode <= 299) {
        	return;
        }
        let body = response.body || "";
    	throw new ApiException<string>(response.httpStatusCode, "Unknown API Status Code!\nBody: \"" + body + "\"");
    }
			
	/**
	 * Unwraps the actual response sent by the server from the response context and deserializes the response content 
	 * to the expected objects
	 * 
	 * @params response Response returned by the server for a request to  
	 * @throws ApiException if the response code was not in [200, 299]
	 */
    public deletePet(response: ResponseContext):   void  {      
        if (isCodeInRange("400", response.httpStatusCode)) {
            throw new ApiException<string>(response.httpStatusCode, "Invalid pet value");
        }
        
        // Work around for incorrect api specification in petstore.yaml
        if (response.httpStatusCode >= 200 && response.httpStatusCode <= 299) {
        	return;
        }
        let body = response.body || "";
    	throw new ApiException<string>(response.httpStatusCode, "Unknown API Status Code!\nBody: \"" + body + "\"");
    }
			
	/**
	 * Unwraps the actual response sent by the server from the response context and deserializes the response content 
	 * to the expected objects
	 * 
	 * @params response Response returned by the server for a request to  
	 * @throws ApiException if the response code was not in [200, 299]
	 */
    public findPetsByStatus(response: ResponseContext):  Array<Pet>  {      
        if (isCodeInRange("200", response.httpStatusCode)) {
            const jsonBody = JSON.parse(response.body);
            const body: Array<Pet> = ObjectSerializer.deserialize(jsonBody, "Array<Pet>", "") as Array<Pet>;            
            return body;
        }
        if (isCodeInRange("400", response.httpStatusCode)) {
            throw new ApiException<string>(response.httpStatusCode, "Invalid status value");
        }
        
        // Work around for incorrect api specification in petstore.yaml
        if (response.httpStatusCode >= 200 && response.httpStatusCode <= 299) {
            const jsonBody = JSON.parse(response.body);
            const body: Array<Pet> = ObjectSerializer.deserialize(jsonBody, "Array<Pet>", "") as Array<Pet>;            
			return body;        		
        }
        let body = response.body || "";
    	throw new ApiException<string>(response.httpStatusCode, "Unknown API Status Code!\nBody: \"" + body + "\"");
    }
			
	/**
	 * Unwraps the actual response sent by the server from the response context and deserializes the response content 
	 * to the expected objects
	 * 
	 * @params response Response returned by the server for a request to  
	 * @throws ApiException if the response code was not in [200, 299]
	 */
    public findPetsByTags(response: ResponseContext):  Array<Pet>  {      
        if (isCodeInRange("200", response.httpStatusCode)) {
            const jsonBody = JSON.parse(response.body);
            const body: Array<Pet> = ObjectSerializer.deserialize(jsonBody, "Array<Pet>", "") as Array<Pet>;            
            return body;
        }
        if (isCodeInRange("400", response.httpStatusCode)) {
            throw new ApiException<string>(response.httpStatusCode, "Invalid tag value");
        }
        
        // Work around for incorrect api specification in petstore.yaml
        if (response.httpStatusCode >= 200 && response.httpStatusCode <= 299) {
            const jsonBody = JSON.parse(response.body);
            const body: Array<Pet> = ObjectSerializer.deserialize(jsonBody, "Array<Pet>", "") as Array<Pet>;            
			return body;        		
        }
        let body = response.body || "";
    	throw new ApiException<string>(response.httpStatusCode, "Unknown API Status Code!\nBody: \"" + body + "\"");
    }
			
	/**
	 * Unwraps the actual response sent by the server from the response context and deserializes the response content 
	 * to the expected objects
	 * 
	 * @params response Response returned by the server for a request to  
	 * @throws ApiException if the response code was not in [200, 299]
	 */
    public getPetById(response: ResponseContext):  Pet  {      
        if (isCodeInRange("200", response.httpStatusCode)) {
            const jsonBody = JSON.parse(response.body);
            const body: Pet = ObjectSerializer.deserialize(jsonBody, "Pet", "") as Pet;            
            return body;
        }
        if (isCodeInRange("400", response.httpStatusCode)) {
            throw new ApiException<string>(response.httpStatusCode, "Invalid ID supplied");
        }
        if (isCodeInRange("404", response.httpStatusCode)) {
            throw new ApiException<string>(response.httpStatusCode, "Pet not found");
        }
        
        // Work around for incorrect api specification in petstore.yaml
        if (response.httpStatusCode >= 200 && response.httpStatusCode <= 299) {
            const jsonBody = JSON.parse(response.body);
            const body: Pet = ObjectSerializer.deserialize(jsonBody, "Pet", "") as Pet;            
			return body;        		
        }
        let body = response.body || "";
    	throw new ApiException<string>(response.httpStatusCode, "Unknown API Status Code!\nBody: \"" + body + "\"");
    }
			
	/**
	 * Unwraps the actual response sent by the server from the response context and deserializes the response content 
	 * to the expected objects
	 * 
	 * @params response Response returned by the server for a request to  
	 * @throws ApiException if the response code was not in [200, 299]
	 */
    public updatePet(response: ResponseContext):   void  {      
        if (isCodeInRange("400", response.httpStatusCode)) {
            throw new ApiException<string>(response.httpStatusCode, "Invalid ID supplied");
        }
        if (isCodeInRange("404", response.httpStatusCode)) {
            throw new ApiException<string>(response.httpStatusCode, "Pet not found");
        }
        if (isCodeInRange("405", response.httpStatusCode)) {
            throw new ApiException<string>(response.httpStatusCode, "Validation exception");
        }
        
        // Work around for incorrect api specification in petstore.yaml
        if (response.httpStatusCode >= 200 && response.httpStatusCode <= 299) {
        	return;
        }
        let body = response.body || "";
    	throw new ApiException<string>(response.httpStatusCode, "Unknown API Status Code!\nBody: \"" + body + "\"");
    }
			
	/**
	 * Unwraps the actual response sent by the server from the response context and deserializes the response content 
	 * to the expected objects
	 * 
	 * @params response Response returned by the server for a request to  
	 * @throws ApiException if the response code was not in [200, 299]
	 */
    public updatePetWithForm(response: ResponseContext):   void  {      
        if (isCodeInRange("405", response.httpStatusCode)) {
            throw new ApiException<string>(response.httpStatusCode, "Invalid input");
        }
        
        // Work around for incorrect api specification in petstore.yaml
        if (response.httpStatusCode >= 200 && response.httpStatusCode <= 299) {
        	return;
        }
        let body = response.body || "";
    	throw new ApiException<string>(response.httpStatusCode, "Unknown API Status Code!\nBody: \"" + body + "\"");
    }
			
	/**
	 * Unwraps the actual response sent by the server from the response context and deserializes the response content 
	 * to the expected objects
	 * 
	 * @params response Response returned by the server for a request to  
	 * @throws ApiException if the response code was not in [200, 299]
	 */
    public uploadFile(response: ResponseContext):  ApiResponse  {      
        if (isCodeInRange("200", response.httpStatusCode)) {
            const jsonBody = JSON.parse(response.body);
            const body: ApiResponse = ObjectSerializer.deserialize(jsonBody, "ApiResponse", "") as ApiResponse;            
            return body;
        }
        
        // Work around for incorrect api specification in petstore.yaml
        if (response.httpStatusCode >= 200 && response.httpStatusCode <= 299) {
            const jsonBody = JSON.parse(response.body);
            const body: ApiResponse = ObjectSerializer.deserialize(jsonBody, "ApiResponse", "") as ApiResponse;            
			return body;        		
        }
        let body = response.body || "";
    	throw new ApiException<string>(response.httpStatusCode, "Unknown API Status Code!\nBody: \"" + body + "\"");
    }
			
}
