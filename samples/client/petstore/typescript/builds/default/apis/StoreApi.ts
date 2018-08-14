// TODO: better import syntax?
import { BaseApiRequestFactory } from './baseapi';
import { RequestContext } from '../http/http';
import { Order } from '../models/Order';




/**
 * StoreApi - interface
 * @export
 * @interface StoreApi
 */
export class StoreApiRequestFactory {

    /**
     * For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors
     * @summary Delete purchase order by ID
     * @param {string} orderId ID of the order that needs to be deleted
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof StoreApiInterface
     */
    public deleteOrder(orderId: string, options?: any): RequestContext {
    	
    	
    	return null;
    }
			
    /**
     * Returns a map of status codes to quantities
     * @summary Returns pet inventories by status
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof StoreApiInterface
     */
    public getInventory(options?: any): RequestContext {
    	
    	
    	return null;
    }
			
    /**
     * For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions
     * @summary Find purchase order by ID
     * @param {number} orderId ID of pet that needs to be fetched
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof StoreApiInterface
     */
    public getOrderById(orderId: number, options?: any): RequestContext {
    	
    	
    	return null;
    }
			
    /**
     * 
     * @summary Place an order for a pet
     * @param {Order} order order placed for purchasing the pet
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof StoreApiInterface
     */
    public placeOrder(order: Order, options?: any): RequestContext {
    	
    	
    	return null;
    }
			
}
