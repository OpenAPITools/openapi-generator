import type { Configuration } from "../configuration";
import type { HttpFile, RequestContext, ResponseContext, HttpInfo } from "../http/http";

import { Order } from "../models/Order";

export abstract class AbstractStoreApiRequestFactory {
    public abstract deleteOrder(orderId: string, options?: Configuration): Promise<RequestContext>;

    public abstract getInventory(options?: Configuration): Promise<RequestContext>;

    public abstract getOrderById(orderId: number, options?: Configuration): Promise<RequestContext>;

    public abstract placeOrder(order: Order, options?: Configuration): Promise<RequestContext>;

}


export abstract class AbstractStoreApiResponseProcessor {
     public abstract deleteOrderWithHttpInfo(response: ResponseContext): Promise<HttpInfo< void>>;

     public abstract getInventoryWithHttpInfo(response: ResponseContext): Promise<HttpInfo<{ [key: string]: number; } >>;

     public abstract getOrderByIdWithHttpInfo(response: ResponseContext): Promise<HttpInfo<Order >>;

     public abstract placeOrderWithHttpInfo(response: ResponseContext): Promise<HttpInfo<Order >>;

}
