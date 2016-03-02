/// <reference path="api.d.ts" />

namespace API.Client {
    'use strict';

    export interface Order {

        "id"?: number;

        "petId"?: number;

        "quantity"?: number;

        "shipDate"?: Date;

        /**
         * Order Status
         */
        "status"?: Order.StatusEnum;

        "complete"?: boolean;
    }

    export namespace Order {

        export enum StatusEnum { 
            placed = <any> 'placed',
            approved = <any> 'approved',
            delivered = <any> 'delivered'
        }
    }
}
