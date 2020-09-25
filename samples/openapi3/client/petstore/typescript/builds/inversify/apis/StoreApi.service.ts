import type { Configuration } from "../configuration";
import type { HttpFile, RequestContext, ResponseContext } from "../http/http";

import { Order } from "../models/Order";

export abstract class AbstractStoreApiRequestFactory {
    public abstract deleteOrder(orderId: string, options?: Configuration): Promise<RequestContext>;

    public abstract getInventory(options?: Configuration): Promise<RequestContext>;

    public abstract getOrderById(orderId: number, options?: Configuration): Promise<RequestContext>;

    public abstract placeOrder(order: Order, options?: Configuration): Promise<RequestContext>;

}


export abstract class AbstractStoreApiResponseProcessor {
     public abstract deleteOrder(response: ResponseContext): Promise< void>;

     public abstract getInventory(response: ResponseContext): Promise<{ [key: string]: number; } >;

     public abstract getOrderById(response: ResponseContext): Promise<Order >;

     public abstract placeOrder(response: ResponseContext): Promise<Order >;

}
