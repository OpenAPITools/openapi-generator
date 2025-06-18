/* tslint:disable */
/* eslint-disable */
/**
 * 
 * @export
 * @interface User
 */
export interface User {
    /**
     * The user ID.
     * @type {number}
     * @memberof User
     */
    id?: number;
    /**
     * The user name.
     * @type {string}
     * @memberof User
     */
    name?: string;
    /**
     * The user email address.
     * @type {string}
     * @memberof User
     */
    email?: string;
    /**
     * 
     * @type {Record<string, unknown>}
     * @memberof User
     */
    metadata?: Record<string, unknown>;
}
/**
 * 
 * @export
 * @interface UserSummary
 */
export interface UserSummary {
    /**
     * The user name.
     * @type {string}
     * @memberof UserSummary
     */
    name?: string;
}
