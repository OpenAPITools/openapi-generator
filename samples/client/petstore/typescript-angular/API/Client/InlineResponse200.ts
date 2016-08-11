/// <reference path="api.d.ts" />

namespace API.Client {
    'use strict';

    export interface InlineResponse200 {

        "tags"?: Array<Tag>;

        "id": number;

        "category"?: any;

        /**
         * pet status in the store
         */
        "status"?: InlineResponse200.StatusEnum;

        "name"?: string;

        "photoUrls"?: Array<string>;
    }

    export namespace InlineResponse200 {

        export enum StatusEnum { 
            available = <any> 'available',
            pending = <any> 'pending',
            sold = <any> 'sold'
        }
    }
}
