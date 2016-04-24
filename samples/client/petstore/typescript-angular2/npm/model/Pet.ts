'use strict';
import * as models from './models';

export interface Pet {

    id?: number;

    category?: models.Category;

    name?: string;

    photoUrls?: Array<string>;

    tags?: Array<models.Tag>;

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
