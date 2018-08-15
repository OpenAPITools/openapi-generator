// TODO: better import syntax?
import { BaseAPIRequestFactory, RequiredError } from './baseapi';
import { RequestContext, HttpMethod } from '../http/http';
import {ObjectSerializer} from '../models/ObjectSerializer';
import { ApiResponse } from '../models/ApiResponse';
import { Pet } from '../models/Pet';

export class PetApiRequestFactory extends BaseAPIRequestFactory {

    public addPet(pet: Pet, options?: any): RequestContext {
        // verify required parameter 'pet' is not null or undefined
        if (pet === null || pet === undefined) {
            throw new RequiredError('Required parameter pet was null or undefined when calling addPet.');
        }

		
		// Path Params
    	const localVarPath = '/pet';

		// Make Request Context
    	const requestContext = this.configuration.baseServer.makeRequestContext(localVarPath, HttpMethod.POST);
            
       
       // Form Params
              	
       	
    	
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
            
		requestContext.setHeaderParam("", ObjectSerializer.serialize(apiKey, "string"));
       
       // Form Params
              	
       	
    	
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
            
        if (status !== undefined) {
        	requestContext.setQueryParam("", ObjectSerializer.serialize(status, "Array<'available' | 'pending' | 'sold'>"));
        }
		
       
       // Form Params
              	
       	
    	
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
            
        if (tags !== undefined) {
        	requestContext.setQueryParam("", ObjectSerializer.serialize(tags, "Array<string>"));
        }
		
       
       // Form Params
              	
       	
    	
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
            
       
       // Form Params
              	
       	
    	
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
            
       
       // Form Params
              	
       	
    	
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
            
       
       // Form Params
              	
       	
    	
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
            
       
       // Form Params
              	
       	
    	
		let authMethod = null;

    	// Apply auth methods
    	authMethod = this.configuration.authMethods["petstore_auth"]
    	if (authMethod) {
    		authMethod.applySecurityAuthentication(requestContext);
    	}
    	
    	return requestContext;
    }
			
}
