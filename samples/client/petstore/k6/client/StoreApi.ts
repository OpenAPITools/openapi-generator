// @ts-ignore
import http from 'k6/http';
// @ts-ignore
import { FileData } from 'k6/http';

import { Order } from './Order.js';

export class StoreApi {

    constructor(private baseUrl: string) {}

    public deleteOrder(orderId: string) {
        const url: string = `${this.baseUrl}/store/order/${orderId}`;

        return http.delete(url);
    }
    public getInventory() {
        const url: string = `${this.baseUrl}/store/inventory`;

        return http.get(url);
    }
    public getOrderById(orderId: number) {
        const url: string = `${this.baseUrl}/store/order/${orderId}`;

        return http.get(url);
    }
    public placeOrder(order: Order) {
        const url: string = `${this.baseUrl}/store/order`;

        const headers = {
            'Content-Type': `application/json`
        };

        return http.post(url, order, { headers });
    }
}
