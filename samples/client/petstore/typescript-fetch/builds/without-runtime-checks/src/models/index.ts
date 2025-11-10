/* tslint:disable */
/* eslint-disable */
/**
 * A category for a pet
 * @export
 * @interface Category
 */
export interface Category {
    /**
     * 
     * @type {number}
     * @memberof Category
     */
    id?: number | undefined;
    /**
     * 
     * @type {string}
     * @memberof Category
     */
    name?: string | undefined;
}
/**
 * Describes the result of uploading an image resource
 * @export
 * @interface ModelApiResponse
 */
export interface ModelApiResponse {
    /**
     * 
     * @type {number}
     * @memberof ModelApiResponse
     */
    code?: number | undefined;
    /**
     * 
     * @type {string}
     * @memberof ModelApiResponse
     */
    type?: string | undefined;
    /**
     * 
     * @type {string}
     * @memberof ModelApiResponse
     */
    message?: string | undefined;
}
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
    id?: number | undefined;
    /**
     * 
     * @type {number}
     * @memberof Order
     */
    petId?: number | undefined;
    /**
     * 
     * @type {number}
     * @memberof Order
     */
    quantity?: number | undefined;
    /**
     * 
     * @type {string}
     * @memberof Order
     */
    shipDate?: string | undefined;
    /**
     * Order Status
     * @type {string}
     * @memberof Order
     */
    status?: OrderStatusEnum | undefined;
    /**
     * 
     * @type {boolean}
     * @memberof Order
     */
    complete?: boolean | undefined;
}


/**
 * @export
 */
export const OrderStatusEnum = {
    Placed: 'placed',
    Approved: 'approved',
    Delivered: 'delivered'
} as const;
export type OrderStatusEnum = typeof OrderStatusEnum[keyof typeof OrderStatusEnum];

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
    id?: number | undefined;
    /**
     * 
     * @type {Category}
     * @memberof Pet
     */
    category?: Category | undefined;
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
    tags?: Array<Tag> | undefined;
    /**
     * pet status in the store
     * @type {string}
     * @memberof Pet
     */
    status?: PetStatusEnum | undefined;
}


/**
 * @export
 */
export const PetStatusEnum = {
    Available: 'available',
    Pending: 'pending',
    Sold: 'sold'
} as const;
export type PetStatusEnum = typeof PetStatusEnum[keyof typeof PetStatusEnum];

/**
 * A tag for a pet
 * @export
 * @interface Tag
 */
export interface Tag {
    /**
     * 
     * @type {number}
     * @memberof Tag
     */
    id?: number | undefined;
    /**
     * 
     * @type {string}
     * @memberof Tag
     */
    name?: string | undefined;
}
/**
 * A User who is purchasing from the pet store
 * @export
 * @interface User
 */
export interface User {
    /**
     * 
     * @type {number}
     * @memberof User
     */
    id?: number | undefined;
    /**
     * 
     * @type {string}
     * @memberof User
     */
    username?: string | undefined;
    /**
     * 
     * @type {string}
     * @memberof User
     */
    firstName?: string | undefined;
    /**
     * 
     * @type {string}
     * @memberof User
     */
    lastName?: string | undefined;
    /**
     * 
     * @type {string}
     * @memberof User
     */
    email?: string | undefined;
    /**
     * 
     * @type {string}
     * @memberof User
     */
    password?: string | undefined;
    /**
     * 
     * @type {string}
     * @memberof User
     */
    phone?: string | undefined;
    /**
     * User Status
     * @type {number}
     * @memberof User
     */
    userStatus?: number | undefined;
}
