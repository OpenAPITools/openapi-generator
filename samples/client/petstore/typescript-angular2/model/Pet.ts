'use strict';
import * as model from "./model.d.ts"

export interface Pet {


    id?: number;


    category?: model.Category;


    name?: string;


    photoUrls?: Array<string>;


    tags?: Array<model.Tag>;

    /**
     * pet status in the store
     */

    status?: Pet.StatusEnum;
}

export namespace Pet {

    export enum StatusEnum { 
        available = <any> 'available',
        pending = <any> 'pending',
        sold = <any> 'sold',
    }
}
