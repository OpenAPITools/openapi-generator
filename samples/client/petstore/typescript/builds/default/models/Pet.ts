// tslint:disable
/*
	TODO: LICENSE INFO
*/
import { Category } from './Category';
import { Tag } from './Tag';
/**
 * A pet for sale in the pet store
 * @export
 * @interface Pet
 */
export interface Pet {
    /**
     * 
     * @type {number}
     * @memberof Pet
     */
    id?: number;
    /**
     * 
     * @type {Category}
     * @memberof Pet
     */
    category?: Category;
    /**
     * 
     * @type {string}
     * @memberof Pet
     */
    name: string;
    /**
     * 
     * @type {Array<string>}
     * @memberof Pet
     */
    photoUrls: Array<string>;
    /**
     * 
     * @type {Array<Tag>}
     * @memberof Pet
     */
    tags?: Array<Tag>;
    /**
     * pet status in the store
     * @type {string}
     * @memberof Pet
     */
    status?: Pet.StatusEnum;
}

/**
 * @export
 * @namespace Pet
 */
export namespace Pet {
    /**
     * @export
     * @enum {string}
     */
    export enum StatusEnum {
        Available = <any> 'available',
        Pending = <any> 'pending',
        Sold = <any> 'sold'
    }
}

