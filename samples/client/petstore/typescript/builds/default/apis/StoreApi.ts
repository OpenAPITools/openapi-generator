// TODO: better import syntax?
import { BaseAPIRequestFactory, RequiredError } from './baseapi';
import { RequestContext, HttpMethod, ResponseContext} from '../http/http';
import * as FormData from "form-data";
import {ObjectSerializer} from '../models/ObjectSerializer';
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
	 * @throws  if the httpStatusCode is not in [200, 299]
	 */
    public deleteOrder(response: ResponseContext):   void  {
    	const jsonBody = JSON.parse(response.body);
    	const responseOK = response.httpStatusCode && response.httpStatusCode >= 200 && response.httpStatusCode <= 299;
        // TODO: make this based on status code!
        if (!responseOK) {
        	throw new Error("Invalid status code: " + response.httpStatusCode + "!");
        }
    }
			
	/**
	 *
	 * @throws { [key: string]: number; } if the httpStatusCode is not in [200, 299]
	 */
    public getInventory(response: ResponseContext):  { [key: string]: number; }  {
    	const jsonBody = JSON.parse(response.body);
    	const responseOK = response.httpStatusCode && response.httpStatusCode >= 200 && response.httpStatusCode <= 299;
        const body: { [key: string]: number; } = ObjectSerializer.deserialize(jsonBody, "{ [key: string]: number; }") as { [key: string]: number; };
        if (responseOK) {
			return body;
        } else {
        	// TODO: deal with different errors based on httpStatusCode
        	throw body
        }
    }
			
	/**
	 *
	 * @throws Order if the httpStatusCode is not in [200, 299]
	 */
    public getOrderById(response: ResponseContext):  Order  {
    	const jsonBody = JSON.parse(response.body);
    	const responseOK = response.httpStatusCode && response.httpStatusCode >= 200 && response.httpStatusCode <= 299;
        const body: Order = ObjectSerializer.deserialize(jsonBody, "Order") as Order;
        if (responseOK) {
			return body;
        } else {
        	// TODO: deal with different errors based on httpStatusCode
        	throw body
        }
    }
			
	/**
	 *
	 * @throws Order if the httpStatusCode is not in [200, 299]
	 */
    public placeOrder(response: ResponseContext):  Order  {
    	const jsonBody = JSON.parse(response.body);
    	const responseOK = response.httpStatusCode && response.httpStatusCode >= 200 && response.httpStatusCode <= 299;
        const body: Order = ObjectSerializer.deserialize(jsonBody, "Order") as Order;
        if (responseOK) {
			return body;
        } else {
        	// TODO: deal with different errors based on httpStatusCode
        	throw body
        }
    }
			
}
