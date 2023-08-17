// @ts-ignore
import http from 'k6/http';
// @ts-ignore
import { FileData } from 'k6/http';

import { Order } from './Order.js';

export class StoreApi {

    constructor(private baseUrl: string) {}

    /**
    * @returns { undefined } - 400
    * @returns { undefined } - 404
    */
    public deleteOrder(orderId: string): {
        code: number,
        headers: {
            [key: string]: string,
        },
        body?: undefined,
    } {
        const url: string = `${this.baseUrl}/store/order/${orderId}`;

        const { code, headers: resHeaders, body } = http.delete(url);

        return { code, headers: resHeaders, body: body as undefined };
    }
    /**
    * @returns { map } - 200
    */
    public getInventory(): {
        code: number,
        headers: {
            [key: string]: string,
        },
        body?: integer,
    } {
        const url: string = `${this.baseUrl}/store/inventory`;

        const { code, headers: resHeaders, body } = http.get(url);

        return { code, headers: resHeaders, body: body as integer };
    }
    /**
    * @returns { Order } - 200
    * @returns { undefined } - 400
    * @returns { undefined } - 404
    */
    public getOrderById(orderId: number): {
        code: number,
        headers: {
            [key: string]: string,
        },
        body?: Order | undefined,
    } {
        const url: string = `${this.baseUrl}/store/order/${orderId}`;

        const { code, headers: resHeaders, body } = http.get(url);

        return { code, headers: resHeaders, body: body as Order | undefined };
    }
    /**
    * @returns { Order } - 200
    * @returns { undefined } - 400
    */
    public placeOrder(order: Order): {
        code: number,
        headers: {
            [key: string]: string,
        },
        body?: Order | undefined,
    } {
        const url: string = `${this.baseUrl}/store/order`;

        const reqHeaders = {
            'Content-Type': `application/json`
        };

        const { code, headers: resHeaders, body } = http.post(url, order, { headers: reqHeaders });

        return { code, headers: resHeaders, body: body as Order | undefined };
    }
}
