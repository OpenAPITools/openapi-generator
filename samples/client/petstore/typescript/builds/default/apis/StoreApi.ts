// TODO: better import syntax?
import { BaseAPIRequestFactory, RequiredError } from './baseapi';
import { RequestContext, HttpMethod, ResponseContext, HttpFile} from '../http/http';
import * as FormData from "form-data";
import {ObjectSerializer} from '../models/ObjectSerializer';
import {ApiException} from './exception';
import {isCodeInRange} from '../util';

import { Order } from '../models/Order';

export class StoreApiRequestFactory extends BaseAPIRequestFactory {
	// TODO: allow passing of Configuration via Options (=> overwrites config set for this request factory
	
    public deleteOrder(orderId: string, options?: any): RequestContext {
        // verify required parameter 'orderId' is not null or undefined
        if (orderId === null || orderId === undefined) {
            throw new RequiredError('Required parameter orderId was null or undefined when calling deleteOrder.');
        }

		
		// Path Params
    	const localVarPath = '/store/order/{orderId}'
            .replace('{' + 'orderId' + '}', encodeURIComponent(String(orderId)));

		// Make Request Context
    	const requestContext = this.configuration.baseServer.makeRequestContext(localVarPath, HttpMethod.DELETE);
        requestContext.setHeaderParam("Accept", "application/json")

        // Query Params
	
		// Header Params
	
		// Form Params


		// Body Params
		
    	// Apply auth methods
    	
    	return requestContext;
    }
			
    public getInventory(options?: any): RequestContext {
		
		// Path Params
    	const localVarPath = '/store/inventory';

		// Make Request Context
    	const requestContext = this.configuration.baseServer.makeRequestContext(localVarPath, HttpMethod.GET);
        requestContext.setHeaderParam("Accept", "application/json")

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
			
    public getOrderById(orderId: number, options?: any): RequestContext {
        // verify required parameter 'orderId' is not null or undefined
        if (orderId === null || orderId === undefined) {
            throw new RequiredError('Required parameter orderId was null or undefined when calling getOrderById.');
        }

		
		// Path Params
    	const localVarPath = '/store/order/{orderId}'
            .replace('{' + 'orderId' + '}', encodeURIComponent(String(orderId)));

		// Make Request Context
    	const requestContext = this.configuration.baseServer.makeRequestContext(localVarPath, HttpMethod.GET);
        requestContext.setHeaderParam("Accept", "application/json")

        // Query Params
	
		// Header Params
	
		// Form Params


		// Body Params
		
    	// Apply auth methods
    	
    	return requestContext;
    }
			
    public placeOrder(order: Order, options?: any): RequestContext {
        // verify required parameter 'order' is not null or undefined
        if (order === null || order === undefined) {
            throw new RequiredError('Required parameter order was null or undefined when calling placeOrder.');
        }

		
		// Path Params
    	const localVarPath = '/store/order';

		// Make Request Context
    	const requestContext = this.configuration.baseServer.makeRequestContext(localVarPath, HttpMethod.POST);
        requestContext.setHeaderParam("Accept", "application/json")

        // Query Params
	
		// Header Params
	
		// Form Params


		// Body Params
        requestContext.setHeaderParam("Content-Type", "application/json");
		// TODO: Should this be handled by ObjectSerializer? imo yes => confidential information included in local object should not be sent
        const needsSerialization = (<any>"Order" !== "string") || requestContext.getHeaders()['Content-Type'] === 'application/json';
        const serializedBody = needsSerialization ? JSON.stringify(order || {}) : (order.toString() || ""); // TODO: `toString` call is unnecessary
        requestContext.setBody(serializedBody);
		
    	// Apply auth methods
    	
    	return requestContext;
    }
			
}

// TODO: find way to split these two files (both dependent on apitemplatefiles)



export class StoreApiResponseProcessor {
	
	/**
	 *
	 * @throws ApiException if the response code was not in [200, 299]
	 */
    public deleteOrder(response: ResponseContext):   void  {      
        if (isCodeInRange("400", response.httpStatusCode)) {
            throw new ApiException<string>(response.httpStatusCode, "Invalid ID supplied");
        }
        if (isCodeInRange("404", response.httpStatusCode)) {
            throw new ApiException<string>(response.httpStatusCode, "Order not found");
        }
        
        // Work around for incorrect api specification in petstore.yaml
        if (response.httpStatusCode >= 200 && response.httpStatusCode <= 299) {
        	return;
        }
        let body = response.body || "";
    	throw new ApiException<string>(response.httpStatusCode, "Unknown API Status Code!\nBody: \"" + body + "\"");
    }
			
	/**
	 *
	 * @throws ApiException if the response code was not in [200, 299]
	 */
    public getInventory(response: ResponseContext):  { [key: string]: number; }  {      
        if (isCodeInRange("200", response.httpStatusCode)) {
            const jsonBody = JSON.parse(response.body);
            const body: { [key: string]: number; } = ObjectSerializer.deserialize(jsonBody, "{ [key: string]: number; }") as { [key: string]: number; };            
            return body;
        }
        
        // Work around for incorrect api specification in petstore.yaml
        if (response.httpStatusCode >= 200 && response.httpStatusCode <= 299) {
            const jsonBody = JSON.parse(response.body);
            const body: { [key: string]: number; } = ObjectSerializer.deserialize(jsonBody, "{ [key: string]: number; }") as { [key: string]: number; };            
			return body;        		
        }
        let body = response.body || "";
    	throw new ApiException<string>(response.httpStatusCode, "Unknown API Status Code!\nBody: \"" + body + "\"");
    }
			
	/**
	 *
	 * @throws ApiException if the response code was not in [200, 299]
	 */
    public getOrderById(response: ResponseContext):  Order  {      
        if (isCodeInRange("200", response.httpStatusCode)) {
            const jsonBody = JSON.parse(response.body);
            const body: Order = ObjectSerializer.deserialize(jsonBody, "Order") as Order;            
            return body;
        }
        if (isCodeInRange("400", response.httpStatusCode)) {
            throw new ApiException<string>(response.httpStatusCode, "Invalid ID supplied");
        }
        if (isCodeInRange("404", response.httpStatusCode)) {
            throw new ApiException<string>(response.httpStatusCode, "Order not found");
        }
        
        // Work around for incorrect api specification in petstore.yaml
        if (response.httpStatusCode >= 200 && response.httpStatusCode <= 299) {
            const jsonBody = JSON.parse(response.body);
            const body: Order = ObjectSerializer.deserialize(jsonBody, "Order") as Order;            
			return body;        		
        }
        let body = response.body || "";
    	throw new ApiException<string>(response.httpStatusCode, "Unknown API Status Code!\nBody: \"" + body + "\"");
    }
			
	/**
	 *
	 * @throws ApiException if the response code was not in [200, 299]
	 */
    public placeOrder(response: ResponseContext):  Order  {      
        if (isCodeInRange("200", response.httpStatusCode)) {
            const jsonBody = JSON.parse(response.body);
            const body: Order = ObjectSerializer.deserialize(jsonBody, "Order") as Order;            
            return body;
        }
        if (isCodeInRange("400", response.httpStatusCode)) {
            throw new ApiException<string>(response.httpStatusCode, "Invalid Order");
        }
        
        // Work around for incorrect api specification in petstore.yaml
        if (response.httpStatusCode >= 200 && response.httpStatusCode <= 299) {
            const jsonBody = JSON.parse(response.body);
            const body: Order = ObjectSerializer.deserialize(jsonBody, "Order") as Order;            
			return body;        		
        }
        let body = response.body || "";
    	throw new ApiException<string>(response.httpStatusCode, "Unknown API Status Code!\nBody: \"" + body + "\"");
    }
			
}
