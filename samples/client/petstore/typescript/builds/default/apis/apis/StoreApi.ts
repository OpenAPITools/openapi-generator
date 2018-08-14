// TODO: better import syntax?

import { Order } from '../';
/**
 * StoreApi - interface
 * @export
 * @interface StoreApi
 */
export interface StoreApiInterface {
    /**
     * For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors
     * @summary Delete purchase order by ID
     * @param {string} orderId ID of the order that needs to be deleted
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof StoreApiInterface
     */
    deleteOrder(orderId: string, options?: any): Promise<{}>;

    /**
     * Returns a map of status codes to quantities
     * @summary Returns pet inventories by status
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof StoreApiInterface
     */
    getInventory(options?: any): Promise<{ [key: string]: number; }>;

    /**
     * For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions
     * @summary Find purchase order by ID
     * @param {number} orderId ID of pet that needs to be fetched
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof StoreApiInterface
     */
    getOrderById(orderId: number, options?: any): Promise<Order>;

    /**
     * 
     * @summary Place an order for a pet
     * @param {Order} order order placed for purchasing the pet
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof StoreApiInterface
     */
    placeOrder(order: Order, options?: any): Promise<Order>;

}
