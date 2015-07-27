/// <reference path="api.d.ts" />

module API.Client {
    'use strict';

    export class Order {

        id: number;

        petId: number;

        quantity: number;

        shipDate: Date;

        /**
         * Order Status
         */
        status: Order.StatusEnum;

        complete: boolean;
    }

    export module Order {

        export enum StatusEnum {  
            placed = <any> 'placed', 
            approved = <any> 'approved', 
            delivered = <any> 'delivered',
        }
    }
}