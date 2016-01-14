/// <reference path="api.d.ts" />

namespace API.Client {
    'use strict';

    export interface Pet {

        id?: number;

        category?: Category;

        name: string;

        photoUrls: Array<string>;

        tags?: Array<Tag>;

        /**
         * pet status in the store
         */
        status?: Pet.StatusEnum;
    }

    export namespace Pet {

        export enum StatusEnum { 
            available = <any> 'available',
            pending = <any> 'pending',
            sold = <any> 'sold'
        }
    }
}
