// tslint:disable
/*
	TODO: LICENSE INFO
*/
/**
 * An order for a pets from the pet store
 * @export
 * @interface Order
 */
export interface Order {
    /**
     * 
     * @type {number}
     * @memberof Order
     */
    id?: number;
    /**
     * 
     * @type {number}
     * @memberof Order
     */
    petId?: number;
    /**
     * 
     * @type {number}
     * @memberof Order
     */
    quantity?: number;
    /**
     * 
     * @type {Date}
     * @memberof Order
     */
    shipDate?: Date;
    /**
     * Order Status
     * @type {string}
     * @memberof Order
     */
    status?: Order.StatusEnum;
    /**
     * 
     * @type {boolean}
     * @memberof Order
     */
    complete?: boolean;
}

/**
 * @export
 * @namespace Order
 */
export namespace Order {
    /**
     * @export
     * @enum {string}
     */
    export enum StatusEnum {
        Placed = <any> 'placed',
        Approved = <any> 'approved',
        Delivered = <any> 'delivered'
    }
}

