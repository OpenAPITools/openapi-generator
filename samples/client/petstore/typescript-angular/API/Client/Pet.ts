/// <reference path="api.d.ts" />

module API.Client {
    'use strict';

    export class Pet {

        id: number;

        category: Category;

        name: string;

        photoUrls: Array<string>;

        tags: Array<Tag>;

        /**
         * pet status in the store
         */
        status: Pet.StatusEnum;
    }

    export module Pet {

        export enum StatusEnum {  
            available = <any> 'available', 
            pending = <any> 'pending', 
            sold = <any> 'sold',
        }
    }
}