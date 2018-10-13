import { BaseAPIRequestFactory } from './baseapi';
import { RequestContext, ResponseContext } from '../http/http';
import { Order } from '../models/Order';
export declare class StoreApiRequestFactory extends BaseAPIRequestFactory {
    deleteOrder(orderId: string, options?: any): RequestContext;
    getInventory(options?: any): RequestContext;
    getOrderById(orderId: number, options?: any): RequestContext;
    placeOrder(order: Order, options?: any): RequestContext;
}
export declare class StoreApiResponseProcessor {
    deleteOrder(response: ResponseContext): void;
    getInventory(response: ResponseContext): {
        [key: string]: number;
    };
    getOrderById(response: ResponseContext): Order;
    placeOrder(response: ResponseContext): Order;
}
