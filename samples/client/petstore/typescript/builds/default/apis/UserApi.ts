// TODO: better import syntax?
import { BaseAPIRequestFactory, RequiredError } from './baseapi';
import { RequestContext, HttpMethod } from '../http/http';
import {ObjectSerializer} from '../models/ObjectSerializer';
import { User } from '../models/User';

export class UserApiRequestFactory extends BaseAPIRequestFactory {

    public createUser(user: User, options?: any): RequestContext {
        // verify required parameter 'user' is not null or undefined
        if (user === null || user === undefined) {
            throw new RequiredError('Required parameter user was null or undefined when calling createUser.');
        }

		
		// Path Params
    	const localVarPath = '/user';

		// Make Request Context
    	const requestContext = this.configuration.baseServer.makeRequestContext(localVarPath, HttpMethod.POST);
            
       
       // Form Params
              	
       	
    	
		let authMethod = null;

    	// Apply auth methods
    	
    	return requestContext;
    }
			
    public createUsersWithArrayInput(user: Array<User>, options?: any): RequestContext {
        // verify required parameter 'user' is not null or undefined
        if (user === null || user === undefined) {
            throw new RequiredError('Required parameter user was null or undefined when calling createUsersWithArrayInput.');
        }

		
		// Path Params
    	const localVarPath = '/user/createWithArray';

		// Make Request Context
    	const requestContext = this.configuration.baseServer.makeRequestContext(localVarPath, HttpMethod.POST);
            
       
       // Form Params
              	
       	
    	
		let authMethod = null;

    	// Apply auth methods
    	
    	return requestContext;
    }
			
    public createUsersWithListInput(user: Array<User>, options?: any): RequestContext {
        // verify required parameter 'user' is not null or undefined
        if (user === null || user === undefined) {
            throw new RequiredError('Required parameter user was null or undefined when calling createUsersWithListInput.');
        }

		
		// Path Params
    	const localVarPath = '/user/createWithList';

		// Make Request Context
    	const requestContext = this.configuration.baseServer.makeRequestContext(localVarPath, HttpMethod.POST);
            
       
       // Form Params
              	
       	
    	
		let authMethod = null;

    	// Apply auth methods
    	
    	return requestContext;
    }
			
    public deleteUser(username: string, options?: any): RequestContext {
        // verify required parameter 'username' is not null or undefined
        if (username === null || username === undefined) {
            throw new RequiredError('Required parameter username was null or undefined when calling deleteUser.');
        }

		
		// Path Params
    	const localVarPath = '/user/{username}'
            .replace('{' + 'username' + '}', encodeURIComponent(String(username)));

		// Make Request Context
    	const requestContext = this.configuration.baseServer.makeRequestContext(localVarPath, HttpMethod.DELETE);
            
       
       // Form Params
              	
       	
    	
		let authMethod = null;

    	// Apply auth methods
    	
    	return requestContext;
    }
			
    public getUserByName(username: string, options?: any): RequestContext {
        // verify required parameter 'username' is not null or undefined
        if (username === null || username === undefined) {
            throw new RequiredError('Required parameter username was null or undefined when calling getUserByName.');
        }

		
		// Path Params
    	const localVarPath = '/user/{username}'
            .replace('{' + 'username' + '}', encodeURIComponent(String(username)));

		// Make Request Context
    	const requestContext = this.configuration.baseServer.makeRequestContext(localVarPath, HttpMethod.GET);
            
       
       // Form Params
              	
       	
    	
		let authMethod = null;

    	// Apply auth methods
    	
    	return requestContext;
    }
			
    public loginUser(username: string, password: string, options?: any): RequestContext {
        // verify required parameter 'username' is not null or undefined
        if (username === null || username === undefined) {
            throw new RequiredError('Required parameter username was null or undefined when calling loginUser.');
        }

        // verify required parameter 'password' is not null or undefined
        if (password === null || password === undefined) {
            throw new RequiredError('Required parameter password was null or undefined when calling loginUser.');
        }

		
		// Path Params
    	const localVarPath = '/user/login';

		// Make Request Context
    	const requestContext = this.configuration.baseServer.makeRequestContext(localVarPath, HttpMethod.GET);
            
        if (username !== undefined) {
        	requestContext.setQueryParam("", ObjectSerializer.serialize(username, "string"));
        }
		
        if (password !== undefined) {
        	requestContext.setQueryParam("", ObjectSerializer.serialize(password, "string"));
        }
		
       
       // Form Params
              	
       	
    	
		let authMethod = null;

    	// Apply auth methods
    	
    	return requestContext;
    }
			
    public logoutUser(options?: any): RequestContext {
		
		// Path Params
    	const localVarPath = '/user/logout';

		// Make Request Context
    	const requestContext = this.configuration.baseServer.makeRequestContext(localVarPath, HttpMethod.GET);
            
       
       // Form Params
              	
       	
    	
		let authMethod = null;

    	// Apply auth methods
    	
    	return requestContext;
    }
			
    public updateUser(username: string, user: User, options?: any): RequestContext {
        // verify required parameter 'username' is not null or undefined
        if (username === null || username === undefined) {
            throw new RequiredError('Required parameter username was null or undefined when calling updateUser.');
        }

        // verify required parameter 'user' is not null or undefined
        if (user === null || user === undefined) {
            throw new RequiredError('Required parameter user was null or undefined when calling updateUser.');
        }

		
		// Path Params
    	const localVarPath = '/user/{username}'
            .replace('{' + 'username' + '}', encodeURIComponent(String(username)));

		// Make Request Context
    	const requestContext = this.configuration.baseServer.makeRequestContext(localVarPath, HttpMethod.PUT);
            
       
       // Form Params
              	
       	
    	
		let authMethod = null;

    	// Apply auth methods
    	
    	return requestContext;
    }
			
}
