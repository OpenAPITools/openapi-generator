// TODO: better import syntax?
import { BaseAPIRequestFactory, RequiredError } from './baseapi';
import {Configuration} from '../configuration';
import { RequestContext, HttpMethod, ResponseContext, HttpFile} from '../http/http';
import * as FormData from "form-data";
import {ObjectSerializer} from '../models/ObjectSerializer';
import {ApiException} from './exception';
import {isCodeInRange} from '../util';

import { User } from '../models/User';

/**
 * no description
 */
export class UserApiRequestFactory extends BaseAPIRequestFactory {
	
    /**
     * This can only be done by the logged in user.
     * Create user
     * @param body Created user object
     */
    public createUser(body: User, options?: Configuration): RequestContext {
		let config = options || this.configuration;
		
        // verify required parameter 'body' is not null or undefined
        if (body === null || body === undefined) {
            throw new RequiredError('Required parameter body was null or undefined when calling createUser.');
        }

		
		// Path Params
    	const localVarPath = '/user';

		// Make Request Context
    	const requestContext = config.baseServer.makeRequestContext(localVarPath, HttpMethod.POST);
        requestContext.setHeaderParam("Accept", "application/json, */*;q=0.8")

        // Query Params
	
		// Header Params
	
		// Form Params


		// Body Params
        const contentType = ObjectSerializer.getPreferredMediaType([]);
        requestContext.setHeaderParam("Content-Type", contentType);
        const serializedBody = ObjectSerializer.stringify(
            ObjectSerializer.serialize(body, "User", ""),
            contentType
        );
        requestContext.setBody(serializedBody);
		
    	// Apply auth methods
    	
    	return requestContext;
    }
			
    /**
     * Creates list of users with given input array
     * @param body List of user object
     */
    public createUsersWithArrayInput(body: Array<User>, options?: Configuration): RequestContext {
		let config = options || this.configuration;
		
        // verify required parameter 'body' is not null or undefined
        if (body === null || body === undefined) {
            throw new RequiredError('Required parameter body was null or undefined when calling createUsersWithArrayInput.');
        }

		
		// Path Params
    	const localVarPath = '/user/createWithArray';

		// Make Request Context
    	const requestContext = config.baseServer.makeRequestContext(localVarPath, HttpMethod.POST);
        requestContext.setHeaderParam("Accept", "application/json, */*;q=0.8")

        // Query Params
	
		// Header Params
	
		// Form Params


		// Body Params
        const contentType = ObjectSerializer.getPreferredMediaType([]);
        requestContext.setHeaderParam("Content-Type", contentType);
        const serializedBody = ObjectSerializer.stringify(
            ObjectSerializer.serialize(body, "Array<User>", ""),
            contentType
        );
        requestContext.setBody(serializedBody);
		
    	// Apply auth methods
    	
    	return requestContext;
    }
			
    /**
     * Creates list of users with given input array
     * @param body List of user object
     */
    public createUsersWithListInput(body: Array<User>, options?: Configuration): RequestContext {
		let config = options || this.configuration;
		
        // verify required parameter 'body' is not null or undefined
        if (body === null || body === undefined) {
            throw new RequiredError('Required parameter body was null or undefined when calling createUsersWithListInput.');
        }

		
		// Path Params
    	const localVarPath = '/user/createWithList';

		// Make Request Context
    	const requestContext = config.baseServer.makeRequestContext(localVarPath, HttpMethod.POST);
        requestContext.setHeaderParam("Accept", "application/json, */*;q=0.8")

        // Query Params
	
		// Header Params
	
		// Form Params


		// Body Params
        const contentType = ObjectSerializer.getPreferredMediaType([]);
        requestContext.setHeaderParam("Content-Type", contentType);
        const serializedBody = ObjectSerializer.stringify(
            ObjectSerializer.serialize(body, "Array<User>", ""),
            contentType
        );
        requestContext.setBody(serializedBody);
		
    	// Apply auth methods
    	
    	return requestContext;
    }
			
    /**
     * This can only be done by the logged in user.
     * Delete user
     * @param username The name that needs to be deleted
     */
    public deleteUser(username: string, options?: Configuration): RequestContext {
		let config = options || this.configuration;
		
        // verify required parameter 'username' is not null or undefined
        if (username === null || username === undefined) {
            throw new RequiredError('Required parameter username was null or undefined when calling deleteUser.');
        }

		
		// Path Params
    	const localVarPath = '/user/{username}'
            .replace('{' + 'username' + '}', encodeURIComponent(String(username)));

		// Make Request Context
    	const requestContext = config.baseServer.makeRequestContext(localVarPath, HttpMethod.DELETE);
        requestContext.setHeaderParam("Accept", "application/json, */*;q=0.8")

        // Query Params
	
		// Header Params
	
		// Form Params


		// Body Params
		
    	// Apply auth methods
    	
    	return requestContext;
    }
			
    /**
     * Get user by user name
     * @param username The name that needs to be fetched. Use user1 for testing.
     */
    public getUserByName(username: string, options?: Configuration): RequestContext {
		let config = options || this.configuration;
		
        // verify required parameter 'username' is not null or undefined
        if (username === null || username === undefined) {
            throw new RequiredError('Required parameter username was null or undefined when calling getUserByName.');
        }

		
		// Path Params
    	const localVarPath = '/user/{username}'
            .replace('{' + 'username' + '}', encodeURIComponent(String(username)));

		// Make Request Context
    	const requestContext = config.baseServer.makeRequestContext(localVarPath, HttpMethod.GET);
        requestContext.setHeaderParam("Accept", "application/json, */*;q=0.8")

        // Query Params
	
		// Header Params
	
		// Form Params


		// Body Params
		
    	// Apply auth methods
    	
    	return requestContext;
    }
			
    /**
     * Logs user into the system
     * @param username The user name for login
     * @param password The password for login in clear text
     */
    public loginUser(username: string, password: string, options?: Configuration): RequestContext {
		let config = options || this.configuration;
		
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
    	const requestContext = config.baseServer.makeRequestContext(localVarPath, HttpMethod.GET);
        requestContext.setHeaderParam("Accept", "application/json, */*;q=0.8")

        // Query Params
        if (username !== undefined) {
        	requestContext.setQueryParam("username", ObjectSerializer.serialize(username, "string", ""));
        }
        if (password !== undefined) {
        	requestContext.setQueryParam("password", ObjectSerializer.serialize(password, "string", ""));
        }
	
		// Header Params
	
		// Form Params


		// Body Params
		
    	// Apply auth methods
    	
    	return requestContext;
    }
			
    /**
     * Logs out current logged in user session
     */
    public logoutUser(options?: Configuration): RequestContext {
		let config = options || this.configuration;
		
		// Path Params
    	const localVarPath = '/user/logout';

		// Make Request Context
    	const requestContext = config.baseServer.makeRequestContext(localVarPath, HttpMethod.GET);
        requestContext.setHeaderParam("Accept", "application/json, */*;q=0.8")

        // Query Params
	
		// Header Params
	
		// Form Params


		// Body Params
		
    	// Apply auth methods
    	
    	return requestContext;
    }
			
    /**
     * This can only be done by the logged in user.
     * Updated user
     * @param username name that need to be deleted
     * @param body Updated user object
     */
    public updateUser(username: string, body: User, options?: Configuration): RequestContext {
		let config = options || this.configuration;
		
        // verify required parameter 'username' is not null or undefined
        if (username === null || username === undefined) {
            throw new RequiredError('Required parameter username was null or undefined when calling updateUser.');
        }

		
        // verify required parameter 'body' is not null or undefined
        if (body === null || body === undefined) {
            throw new RequiredError('Required parameter body was null or undefined when calling updateUser.');
        }

		
		// Path Params
    	const localVarPath = '/user/{username}'
            .replace('{' + 'username' + '}', encodeURIComponent(String(username)));

		// Make Request Context
    	const requestContext = config.baseServer.makeRequestContext(localVarPath, HttpMethod.PUT);
        requestContext.setHeaderParam("Accept", "application/json, */*;q=0.8")

        // Query Params
	
		// Header Params
	
		// Form Params


		// Body Params
        const contentType = ObjectSerializer.getPreferredMediaType([]);
        requestContext.setHeaderParam("Content-Type", contentType);
        const serializedBody = ObjectSerializer.stringify(
            ObjectSerializer.serialize(body, "User", ""),
            contentType
        );
        requestContext.setBody(serializedBody);
		
    	// Apply auth methods
    	
    	return requestContext;
    }
			
}



export class UserApiResponseProcessor {

    /**
     * Unwraps the actual response sent by the server from the response context and deserializes the response content
     * to the expected objects
     *
     * @params response Response returned by the server for a request to createUser
     * @throws ApiException if the response code was not in [200, 299]
     */
    public createUser(response: ResponseContext):   void  {      
        const contentType = ObjectSerializer.normalizeMediaType(response.headers["content-type"]);
        if (isCodeInRange("0", response.httpStatusCode)) {
            throw new ApiException<string>(response.httpStatusCode, "successful operation");
        }

        // Work around for missing responses in specification, e.g. for petstore.yaml
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
     * @params response Response returned by the server for a request to createUsersWithArrayInput
     * @throws ApiException if the response code was not in [200, 299]
     */
    public createUsersWithArrayInput(response: ResponseContext):   void  {      
        const contentType = ObjectSerializer.normalizeMediaType(response.headers["content-type"]);
        if (isCodeInRange("0", response.httpStatusCode)) {
            throw new ApiException<string>(response.httpStatusCode, "successful operation");
        }

        // Work around for missing responses in specification, e.g. for petstore.yaml
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
     * @params response Response returned by the server for a request to createUsersWithListInput
     * @throws ApiException if the response code was not in [200, 299]
     */
    public createUsersWithListInput(response: ResponseContext):   void  {      
        const contentType = ObjectSerializer.normalizeMediaType(response.headers["content-type"]);
        if (isCodeInRange("0", response.httpStatusCode)) {
            throw new ApiException<string>(response.httpStatusCode, "successful operation");
        }

        // Work around for missing responses in specification, e.g. for petstore.yaml
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
     * @params response Response returned by the server for a request to deleteUser
     * @throws ApiException if the response code was not in [200, 299]
     */
    public deleteUser(response: ResponseContext):   void  {      
        const contentType = ObjectSerializer.normalizeMediaType(response.headers["content-type"]);
        if (isCodeInRange("400", response.httpStatusCode)) {
            throw new ApiException<string>(response.httpStatusCode, "Invalid username supplied");
        }
        if (isCodeInRange("404", response.httpStatusCode)) {
            throw new ApiException<string>(response.httpStatusCode, "User not found");
        }

        // Work around for missing responses in specification, e.g. for petstore.yaml
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
     * @params response Response returned by the server for a request to getUserByName
     * @throws ApiException if the response code was not in [200, 299]
     */
    public getUserByName(response: ResponseContext):  User  {      
        const contentType = ObjectSerializer.normalizeMediaType(response.headers["content-type"]);
        if (isCodeInRange("200", response.httpStatusCode)) {
            let body: User;
            if ("User" as string === "HttpFile" as string) {
                body = response.getBodyAsFile() as any as User;
            } else {
                body = ObjectSerializer.deserialize(
                    ObjectSerializer.parse(response.body, contentType),
                    "User", ""
                ) as User;
            }
            return body;
        }
        if (isCodeInRange("400", response.httpStatusCode)) {
            throw new ApiException<string>(response.httpStatusCode, "Invalid username supplied");
        }
        if (isCodeInRange("404", response.httpStatusCode)) {
            throw new ApiException<string>(response.httpStatusCode, "User not found");
        }

        // Work around for missing responses in specification, e.g. for petstore.yaml
        if (response.httpStatusCode >= 200 && response.httpStatusCode <= 299) {
            let body: User;
            if ("User" as string === "HttpFile" as string) {
                body = response.getBodyAsFile() as any as User;
            } else {
                body = ObjectSerializer.deserialize(
                    ObjectSerializer.parse(response.body, contentType),
                    "User", ""
                ) as User;
            }
            return body;
        }

        let body = response.body || "";
    	throw new ApiException<string>(response.httpStatusCode, "Unknown API Status Code!\nBody: \"" + body + "\"");
    }
			
    /**
     * Unwraps the actual response sent by the server from the response context and deserializes the response content
     * to the expected objects
     *
     * @params response Response returned by the server for a request to loginUser
     * @throws ApiException if the response code was not in [200, 299]
     */
    public loginUser(response: ResponseContext):  string  {      
        const contentType = ObjectSerializer.normalizeMediaType(response.headers["content-type"]);
        if (isCodeInRange("200", response.httpStatusCode)) {
            let body: string;
            if ("string" as string === "HttpFile" as string) {
                body = response.getBodyAsFile() as any as string;
            } else {
                body = ObjectSerializer.deserialize(
                    ObjectSerializer.parse(response.body, contentType),
                    "string", ""
                ) as string;
            }
            return body;
        }
        if (isCodeInRange("400", response.httpStatusCode)) {
            throw new ApiException<string>(response.httpStatusCode, "Invalid username/password supplied");
        }

        // Work around for missing responses in specification, e.g. for petstore.yaml
        if (response.httpStatusCode >= 200 && response.httpStatusCode <= 299) {
            let body: string;
            if ("string" as string === "HttpFile" as string) {
                body = response.getBodyAsFile() as any as string;
            } else {
                body = ObjectSerializer.deserialize(
                    ObjectSerializer.parse(response.body, contentType),
                    "string", ""
                ) as string;
            }
            return body;
        }

        let body = response.body || "";
    	throw new ApiException<string>(response.httpStatusCode, "Unknown API Status Code!\nBody: \"" + body + "\"");
    }
			
    /**
     * Unwraps the actual response sent by the server from the response context and deserializes the response content
     * to the expected objects
     *
     * @params response Response returned by the server for a request to logoutUser
     * @throws ApiException if the response code was not in [200, 299]
     */
    public logoutUser(response: ResponseContext):   void  {      
        const contentType = ObjectSerializer.normalizeMediaType(response.headers["content-type"]);
        if (isCodeInRange("0", response.httpStatusCode)) {
            throw new ApiException<string>(response.httpStatusCode, "successful operation");
        }

        // Work around for missing responses in specification, e.g. for petstore.yaml
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
     * @params response Response returned by the server for a request to updateUser
     * @throws ApiException if the response code was not in [200, 299]
     */
    public updateUser(response: ResponseContext):   void  {      
        const contentType = ObjectSerializer.normalizeMediaType(response.headers["content-type"]);
        if (isCodeInRange("400", response.httpStatusCode)) {
            throw new ApiException<string>(response.httpStatusCode, "Invalid user supplied");
        }
        if (isCodeInRange("404", response.httpStatusCode)) {
            throw new ApiException<string>(response.httpStatusCode, "User not found");
        }

        // Work around for missing responses in specification, e.g. for petstore.yaml
        if (response.httpStatusCode >= 200 && response.httpStatusCode <= 299) {
            return;
        }

        let body = response.body || "";
    	throw new ApiException<string>(response.httpStatusCode, "Unknown API Status Code!\nBody: \"" + body + "\"");
    }
			
}
