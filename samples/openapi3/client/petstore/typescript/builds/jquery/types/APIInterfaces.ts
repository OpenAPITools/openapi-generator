import { ResponseContext, RequestContext, HttpFile } from '../http/http';
import * as models from '../models/all';
import { Configuration} from '../configuration'
import { Observable, of, from } from '../rxjsStub';
import {mergeMap, map} from  '../rxjsStub';

import { ApiResponse } from '../models/ApiResponse';
import { Category } from '../models/Category';
import { InlineObject } from '../models/InlineObject';
import { InlineObject1 } from '../models/InlineObject1';
import { Order } from '../models/Order';
import { Pet } from '../models/Pet';
import { Tag } from '../models/Tag';
import { User } from '../models/User';

export interface GenericPetApiInterface<T1, T2, T3, T4, T5, T6, T7, T8> {
    /**
     * Add a new pet to the store
     * @param pet Pet object that needs to be added to the store
     */
    addPet(pet: Pet, options?: Configuration): T1;

    /**
     * Deletes a pet
     * @param petId Pet id to delete
     * @param apiKey 
     */
    deletePet(petId: number, apiKey?: string, options?: Configuration): T2;

    /**
     * Multiple status values can be provided with comma separated strings
     * Finds Pets by status
     * @param status Status values that need to be considered for filter
     */
    findPetsByStatus(status: Array<'available' | 'pending' | 'sold'>, options?: Configuration): T3;

    /**
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     * Finds Pets by tags
     * @param tags Tags to filter by
     */
    findPetsByTags(tags: Array<string>, options?: Configuration): T4;

    /**
     * Returns a single pet
     * Find pet by ID
     * @param petId ID of pet to return
     */
    getPetById(petId: number, options?: Configuration): T5;

    /**
     * Update an existing pet
     * @param pet Pet object that needs to be added to the store
     */
    updatePet(pet: Pet, options?: Configuration): T6;

    /**
     * Updates a pet in the store with form data
     * @param petId ID of pet that needs to be updated
     * @param name Updated name of the pet
     * @param status Updated status of the pet
     */
    updatePetWithForm(petId: number, name?: string, status?: string, options?: Configuration): T7;

    /**
     * uploads an image
     * @param petId ID of pet to update
     * @param additionalMetadata Additional data to pass to server
     * @param file file to upload
     */
    uploadFile(petId: number, additionalMetadata?: string, file?: HttpFile, options?: Configuration): T8;

}




export interface GenericStoreApiInterface<T1, T2, T3, T4> {
    /**
     * For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors
     * Delete purchase order by ID
     * @param orderId ID of the order that needs to be deleted
     */
    deleteOrder(orderId: string, options?: Configuration): T1;

    /**
     * Returns a map of status codes to quantities
     * Returns pet inventories by status
     */
    getInventory(options?: Configuration): T2;

    /**
     * For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions
     * Find purchase order by ID
     * @param orderId ID of pet that needs to be fetched
     */
    getOrderById(orderId: number, options?: Configuration): T3;

    /**
     * Place an order for a pet
     * @param order order placed for purchasing the pet
     */
    placeOrder(order: Order, options?: Configuration): T4;

}




export interface GenericUserApiInterface<T1, T2, T3, T4, T5, T6, T7, T8> {
    /**
     * This can only be done by the logged in user.
     * Create user
     * @param user Created user object
     */
    createUser(user: User, options?: Configuration): T1;

    /**
     * Creates list of users with given input array
     * @param user List of user object
     */
    createUsersWithArrayInput(user: Array<User>, options?: Configuration): T2;

    /**
     * Creates list of users with given input array
     * @param user List of user object
     */
    createUsersWithListInput(user: Array<User>, options?: Configuration): T3;

    /**
     * This can only be done by the logged in user.
     * Delete user
     * @param username The name that needs to be deleted
     */
    deleteUser(username: string, options?: Configuration): T4;

    /**
     * Get user by user name
     * @param username The name that needs to be fetched. Use user1 for testing.
     */
    getUserByName(username: string, options?: Configuration): T5;

    /**
     * Logs user into the system
     * @param username The user name for login
     * @param password The password for login in clear text
     */
    loginUser(username: string, password: string, options?: Configuration): T6;

    /**
     * Logs out current logged in user session
     */
    logoutUser(options?: Configuration): T7;

    /**
     * This can only be done by the logged in user.
     * Updated user
     * @param username name that need to be deleted
     * @param user Updated user object
     */
    updateUser(username: string, user: User, options?: Configuration): T8;

}



