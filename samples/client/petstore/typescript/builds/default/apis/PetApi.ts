// TODO: better import syntax?
import { BaseAPIRequestFactory, RequiredError } from './baseapi';
import { RequestContext, HttpMethod, ResponseContext} from '../http/http';
import * as FormData from "form-data";
import {ObjectSerializer} from '../models/ObjectSerializer';
import { ApiResponse } from '../models/ApiResponse';
import { Pet } from '../models/Pet';

export class PetApiRequestFactory extends BaseAPIRequestFactory {
	// TODO: allow passing of Configuration via Options (=> overwrites config set for this request factory
	
    public addPet(pet: Pet, options?: any): RequestContext {
        // verify required parameter 'pet' is not null or undefined
        if (pet === null || pet === undefined) {
            throw new RequiredError('Required parameter pet was null or undefined when calling addPet.');
        }

		
		// Path Params
    	const localVarPath = '/pet';

		// Make Request Context
    	const requestContext = this.configuration.baseServer.makeRequestContext(localVarPath, HttpMethod.POST);
            
        // Query Params
	
		// Header Params
	
		// Form Params


		// Body Params
        // TODO: deal with this? Could be useful for server definition
        requestContext.setHeaderParam("Content-Type", "application/json");
		// TODO: Should this be handled by ObjectSerializer? imo yes => confidential information included in local object should not be sent
        const needsSerialization = (<any>"Pet" !== "string") || requestContext.getHeaders()['Content-Type'] === 'application/json';
        const serializedBody = needsSerialization ? JSON.stringify(pet || {}) : (pet.toString() || ""); // TODO: `toString` call is unnecessary
        requestContext.setBody(serializedBody);
		
		let authMethod = null;
    	// Apply auth methods
    	authMethod = this.configuration.authMethods["petstore_auth"]
    	if (authMethod) {
    		authMethod.applySecurityAuthentication(requestContext);
    	}
    	
    	return requestContext;
    }
			
    public deletePet(petId: number, apiKey?: string, options?: any): RequestContext {
        // verify required parameter 'petId' is not null or undefined
        if (petId === null || petId === undefined) {
            throw new RequiredError('Required parameter petId was null or undefined when calling deletePet.');
        }

		
		// Path Params
    	const localVarPath = '/pet/{petId}'
            .replace('{' + 'petId' + '}', encodeURIComponent(String(petId)));

		// Make Request Context
    	const requestContext = this.configuration.baseServer.makeRequestContext(localVarPath, HttpMethod.DELETE);
            
        // Query Params
	
		// Header Params
		requestContext.setHeaderParam("", ObjectSerializer.serialize(apiKey, "string"));
	
		// Form Params


		// Body Params
		
		let authMethod = null;
    	// Apply auth methods
    	authMethod = this.configuration.authMethods["petstore_auth"]
    	if (authMethod) {
    		authMethod.applySecurityAuthentication(requestContext);
    	}
    	
    	return requestContext;
    }
			
    public findPetsByStatus(status: Array<'available' | 'pending' | 'sold'>, options?: any): RequestContext {
        // verify required parameter 'status' is not null or undefined
        if (status === null || status === undefined) {
            throw new RequiredError('Required parameter status was null or undefined when calling findPetsByStatus.');
        }

		
		// Path Params
    	const localVarPath = '/pet/findByStatus';

		// Make Request Context
    	const requestContext = this.configuration.baseServer.makeRequestContext(localVarPath, HttpMethod.GET);
            
        // Query Params
        if (status !== undefined) {
        	requestContext.setQueryParam("", ObjectSerializer.serialize(status, "Array<'available' | 'pending' | 'sold'>"));
        }
	
		// Header Params
	
		// Form Params


		// Body Params
		
		let authMethod = null;
    	// Apply auth methods
    	authMethod = this.configuration.authMethods["petstore_auth"]
    	if (authMethod) {
    		authMethod.applySecurityAuthentication(requestContext);
    	}
    	
    	return requestContext;
    }
			
    public findPetsByTags(tags: Array<string>, options?: any): RequestContext {
        // verify required parameter 'tags' is not null or undefined
        if (tags === null || tags === undefined) {
            throw new RequiredError('Required parameter tags was null or undefined when calling findPetsByTags.');
        }

		
		// Path Params
    	const localVarPath = '/pet/findByTags';

		// Make Request Context
    	const requestContext = this.configuration.baseServer.makeRequestContext(localVarPath, HttpMethod.GET);
            
        // Query Params
        if (tags !== undefined) {
        	requestContext.setQueryParam("", ObjectSerializer.serialize(tags, "Array<string>"));
        }
	
		// Header Params
	
		// Form Params


		// Body Params
		
		let authMethod = null;
    	// Apply auth methods
    	authMethod = this.configuration.authMethods["petstore_auth"]
    	if (authMethod) {
    		authMethod.applySecurityAuthentication(requestContext);
    	}
    	
    	return requestContext;
    }
			
    public getPetById(petId: number, options?: any): RequestContext {
        // verify required parameter 'petId' is not null or undefined
        if (petId === null || petId === undefined) {
            throw new RequiredError('Required parameter petId was null or undefined when calling getPetById.');
        }

		
		// Path Params
    	const localVarPath = '/pet/{petId}'
            .replace('{' + 'petId' + '}', encodeURIComponent(String(petId)));

		// Make Request Context
    	const requestContext = this.configuration.baseServer.makeRequestContext(localVarPath, HttpMethod.GET);
            
        // Query Params
	
		// Header Params
	
		// Form Params


		// Body Params
		
		let authMethod = null;
    	// Apply auth methods
    	authMethod = this.configuration.authMethods["api_key"]
    	if (authMethod) {
    		authMethod.applySecurityAuthentication(requestContext);
    	}
    	
    	return requestContext;
    }
			
    public updatePet(pet: Pet, options?: any): RequestContext {
        // verify required parameter 'pet' is not null or undefined
        if (pet === null || pet === undefined) {
            throw new RequiredError('Required parameter pet was null or undefined when calling updatePet.');
        }

		
		// Path Params
    	const localVarPath = '/pet';

		// Make Request Context
    	const requestContext = this.configuration.baseServer.makeRequestContext(localVarPath, HttpMethod.PUT);
            
        // Query Params
	
		// Header Params
	
		// Form Params


		// Body Params
        // TODO: deal with this? Could be useful for server definition
        requestContext.setHeaderParam("Content-Type", "application/json");
		// TODO: Should this be handled by ObjectSerializer? imo yes => confidential information included in local object should not be sent
        const needsSerialization = (<any>"Pet" !== "string") || requestContext.getHeaders()['Content-Type'] === 'application/json';
        const serializedBody = needsSerialization ? JSON.stringify(pet || {}) : (pet.toString() || ""); // TODO: `toString` call is unnecessary
        requestContext.setBody(serializedBody);
		
		let authMethod = null;
    	// Apply auth methods
    	authMethod = this.configuration.authMethods["petstore_auth"]
    	if (authMethod) {
    		authMethod.applySecurityAuthentication(requestContext);
    	}
    	
    	return requestContext;
    }
			
    public updatePetWithForm(petId: number, name?: string, status?: string, options?: any): RequestContext {
        // verify required parameter 'petId' is not null or undefined
        if (petId === null || petId === undefined) {
            throw new RequiredError('Required parameter petId was null or undefined when calling updatePetWithForm.');
        }

		
		// Path Params
    	const localVarPath = '/pet/{petId}'
            .replace('{' + 'petId' + '}', encodeURIComponent(String(petId)));

		// Make Request Context
    	const requestContext = this.configuration.baseServer.makeRequestContext(localVarPath, HttpMethod.POST);
            
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
    	authMethod = this.configuration.authMethods["petstore_auth"]
    	if (authMethod) {
    		authMethod.applySecurityAuthentication(requestContext);
    	}
    	
    	return requestContext;
    }
			
    public uploadFile(petId: number, additionalMetadata?: string, file?: any, options?: any): RequestContext {
        // verify required parameter 'petId' is not null or undefined
        if (petId === null || petId === undefined) {
            throw new RequiredError('Required parameter petId was null or undefined when calling uploadFile.');
        }

		
		// Path Params
    	const localVarPath = '/pet/{petId}/uploadImage'
            .replace('{' + 'petId' + '}', encodeURIComponent(String(petId)));

		// Make Request Context
    	const requestContext = this.configuration.baseServer.makeRequestContext(localVarPath, HttpMethod.POST);
            
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
            localVarFormParams.append('file', file as any);
        }
		requestContext.setBody(localVarFormParams);

		// Body Params
		
		let authMethod = null;
    	// Apply auth methods
    	authMethod = this.configuration.authMethods["petstore_auth"]
    	if (authMethod) {
    		authMethod.applySecurityAuthentication(requestContext);
    	}
    	
    	return requestContext;
    }
			
}

// TODO: find way to split these two files (both dependent on apitemplatefiles)



export class PetApiResponseProcessor {
	
	/**
	 *
	 * @throws  if the httpStatusCode is not in [200, 299]
	 */
    public addPet(response: ResponseContext):   void  {
    	const jsonBody = JSON.parse(response.body);
    	const responseOK = response.httpStatusCode && response.httpStatusCode >= 200 && response.httpStatusCode <= 299;
        // TODO: make this based on status code!
        if (!responseOK) {
        	throw new Error("Invalid status code: " + response.httpStatusCode + "!");
        }
    }
			
	/**
	 *
	 * @throws  if the httpStatusCode is not in [200, 299]
	 */
    public deletePet(response: ResponseContext):   void  {
    	const jsonBody = JSON.parse(response.body);
    	const responseOK = response.httpStatusCode && response.httpStatusCode >= 200 && response.httpStatusCode <= 299;
        // TODO: make this based on status code!
        if (!responseOK) {
        	throw new Error("Invalid status code: " + response.httpStatusCode + "!");
        }
    }
			
	/**
	 *
	 * @throws Array<Pet> if the httpStatusCode is not in [200, 299]
	 */
    public findPetsByStatus(response: ResponseContext):  Array<Pet>  {
    	const jsonBody = JSON.parse(response.body);
    	const responseOK = response.httpStatusCode && response.httpStatusCode >= 200 && response.httpStatusCode <= 299;
        const body: Array<Pet> = ObjectSerializer.deserialize(jsonBody, "Array<Pet>") as Array<Pet>;
        if (responseOK) {
			return body;
        } else {
        	// TODO: deal with different errors based on httpStatusCode
        	throw body
        }
    }
			
	/**
	 *
	 * @throws Array<Pet> if the httpStatusCode is not in [200, 299]
	 */
    public findPetsByTags(response: ResponseContext):  Array<Pet>  {
    	const jsonBody = JSON.parse(response.body);
    	const responseOK = response.httpStatusCode && response.httpStatusCode >= 200 && response.httpStatusCode <= 299;
        const body: Array<Pet> = ObjectSerializer.deserialize(jsonBody, "Array<Pet>") as Array<Pet>;
        if (responseOK) {
			return body;
        } else {
        	// TODO: deal with different errors based on httpStatusCode
        	throw body
        }
    }
			
	/**
	 *
	 * @throws Pet if the httpStatusCode is not in [200, 299]
	 */
    public getPetById(response: ResponseContext):  Pet  {
    	const jsonBody = JSON.parse(response.body);
    	const responseOK = response.httpStatusCode && response.httpStatusCode >= 200 && response.httpStatusCode <= 299;
        const body: Pet = ObjectSerializer.deserialize(jsonBody, "Pet") as Pet;
        if (responseOK) {
			return body;
        } else {
        	// TODO: deal with different errors based on httpStatusCode
        	throw body
        }
    }
			
	/**
	 *
	 * @throws  if the httpStatusCode is not in [200, 299]
	 */
    public updatePet(response: ResponseContext):   void  {
    	const jsonBody = JSON.parse(response.body);
    	const responseOK = response.httpStatusCode && response.httpStatusCode >= 200 && response.httpStatusCode <= 299;
        // TODO: make this based on status code!
        if (!responseOK) {
        	throw new Error("Invalid status code: " + response.httpStatusCode + "!");
        }
    }
			
	/**
	 *
	 * @throws  if the httpStatusCode is not in [200, 299]
	 */
    public updatePetWithForm(response: ResponseContext):   void  {
    	const jsonBody = JSON.parse(response.body);
    	const responseOK = response.httpStatusCode && response.httpStatusCode >= 200 && response.httpStatusCode <= 299;
        // TODO: make this based on status code!
        if (!responseOK) {
        	throw new Error("Invalid status code: " + response.httpStatusCode + "!");
        }
    }
			
	/**
	 *
	 * @throws ApiResponse if the httpStatusCode is not in [200, 299]
	 */
    public uploadFile(response: ResponseContext):  ApiResponse  {
    	const jsonBody = JSON.parse(response.body);
    	const responseOK = response.httpStatusCode && response.httpStatusCode >= 200 && response.httpStatusCode <= 299;
        const body: ApiResponse = ObjectSerializer.deserialize(jsonBody, "ApiResponse") as ApiResponse;
        if (responseOK) {
			return body;
        } else {
        	// TODO: deal with different errors based on httpStatusCode
        	throw body
        }
    }
			
}
