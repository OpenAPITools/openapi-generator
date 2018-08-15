// TODO: better import syntax?
import { BaseAPIRequestFactory, RequiredError } from './baseapi';
import { RequestContext, HttpMethod } from '../http/http';
import {ObjectSerializer} from '../models/ObjectSerializer';
import { Order } from '../models/Order';

export class StoreApiRequestFactory extends BaseAPIRequestFactory {

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
            
       
       // Form Params
              	
       	
    	
		let authMethod = null;

    	// Apply auth methods
    	
    	return requestContext;
    }
			
    public getInventory(options?: any): RequestContext {
		
		// Path Params
    	const localVarPath = '/store/inventory';

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
            
       
       // Form Params
              	
       	
    	
		let authMethod = null;

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
            
       
       // Form Params
              	
       	
    	
		let authMethod = null;

    	// Apply auth methods
    	
    	return requestContext;
    }
			
}
