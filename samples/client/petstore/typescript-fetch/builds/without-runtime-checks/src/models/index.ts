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
     */
    id?: number;
    /**
     * 
     */
    name?: string;
}
/**
 * Describes the result of uploading an image resource
 * @export
 * @interface ModelApiResponse
 */
export interface ModelApiResponse {
    /**
     * 
     */
    code?: number;
    /**
     * 
     */
    type?: string;
    /**
     * 
     */
    message?: string;
}
/**
 * An order for a pets from the pet store
 * @export
 * @interface Order
 */
export interface Order {
    /**
     * 
     */
    id?: number;
    /**
     * 
     */
    petId?: number;
    /**
     * 
     */
    quantity?: number;
    /**
     * 
     */
    shipDate?: string;
    /**
     * Order Status
     */
    status?: OrderStatusEnum;
    /**
     * 
     */
    complete?: boolean;
}


/**
 * @export
 */
export const OrderStatusEnum = {
    Placed: 'placed',
    Approved: 'approved',
    Delivered: 'delivered',
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
     */
    id?: number;
    /**
     * 
     */
    category?: Category;
    /**
     * 
     */
    name: string;
    /**
     * 
     */
    photoUrls: Array<string>;
    /**
     * 
     */
    tags?: Array<Tag>;
    /**
     * pet status in the store
     */
    status?: PetStatusEnum;
}


/**
 * @export
 */
export const PetStatusEnum = {
    Available: 'available',
    Pending: 'pending',
    Sold: 'sold',
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
     */
    id?: number;
    /**
     * 
     */
    name?: string;
}
/**
 * A User who is purchasing from the pet store
 * @export
 * @interface User
 */
export interface User {
    /**
     * 
     */
    id?: number;
    /**
     * 
     */
    username?: string;
    /**
     * 
     */
    firstName?: string;
    /**
     * 
     */
    lastName?: string;
    /**
     * 
     */
    email?: string;
    /**
     * 
     */
    password?: string;
    /**
     * 
     */
    phone?: string;
    /**
     * User Status
     */
    userStatus?: number;
}
