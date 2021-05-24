import { ResponseContext, RequestContext, HttpFile } from '../http/http';
import * as models from '../models/all';
import { Configuration} from '../configuration'

import { ApiResponse } from '../models/ApiResponse';
import { Category } from '../models/Category';
import { Order } from '../models/Order';
import { Pet } from '../models/Pet';
import { Tag } from '../models/Tag';
import { User } from '../models/User';
import { ObservablePetApi } from './ObservableAPI';

import { PetApiRequestFactory, PetApiResponseProcessor} from "../apis/PetApi";
export class PromisePetApi {
    private api: ObservablePetApi

    public constructor(
        configuration: Configuration,
        requestFactory?: PetApiRequestFactory,
        responseProcessor?: PetApiResponseProcessor
    ) {
        this.api = new ObservablePetApi(configuration, requestFactory, responseProcessor);
    }

    /**
     * Add a new pet to the store
     * @param pet Pet object that needs to be added to the store
     */
    public addPet(pet: Pet, _options?: Configuration): Promise<Pet> {
        const result = this.api.addPet(pet, _options);
        return result.toPromise();
    }

    /**
     * Deletes a pet
     * @param petId Pet id to delete
     * @param apiKey 
     */
    public deletePet(petId: number, apiKey?: string, _options?: Configuration): Promise<void> {
        const result = this.api.deletePet(petId, apiKey, _options);
        return result.toPromise();
    }

    /**
     * Multiple status values can be provided with comma separated strings
     * Finds Pets by status
     * @param status Status values that need to be considered for filter
     */
    public findPetsByStatus(status: Array<'available' | 'pending' | 'sold'>, _options?: Configuration): Promise<Array<Pet>> {
        const result = this.api.findPetsByStatus(status, _options);
        return result.toPromise();
    }

    /**
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     * Finds Pets by tags
     * @param tags Tags to filter by
     */
    public findPetsByTags(tags: Array<string>, _options?: Configuration): Promise<Array<Pet>> {
        const result = this.api.findPetsByTags(tags, _options);
        return result.toPromise();
    }

    /**
     * Returns a single pet
     * Find pet by ID
     * @param petId ID of pet to return
     */
    public getPetById(petId: number, _options?: Configuration): Promise<Pet> {
        const result = this.api.getPetById(petId, _options);
        return result.toPromise();
    }

    /**
     * Update an existing pet
     * @param pet Pet object that needs to be added to the store
     */
    public updatePet(pet: Pet, _options?: Configuration): Promise<Pet> {
        const result = this.api.updatePet(pet, _options);
        return result.toPromise();
    }

    /**
     * Updates a pet in the store with form data
     * @param petId ID of pet that needs to be updated
     * @param name Updated name of the pet
     * @param status Updated status of the pet
     */
    public updatePetWithForm(petId: number, name?: string, status?: string, _options?: Configuration): Promise<void> {
        const result = this.api.updatePetWithForm(petId, name, status, _options);
        return result.toPromise();
    }

    /**
     * uploads an image
     * @param petId ID of pet to update
     * @param additionalMetadata Additional data to pass to server
     * @param file file to upload
     */
    public uploadFile(petId: number, additionalMetadata?: string, file?: HttpFile, _options?: Configuration): Promise<ApiResponse> {
        const result = this.api.uploadFile(petId, additionalMetadata, file, _options);
        return result.toPromise();
    }


}



import { ObservableStoreApi } from './ObservableAPI';

import { StoreApiRequestFactory, StoreApiResponseProcessor} from "../apis/StoreApi";
export class PromiseStoreApi {
    private api: ObservableStoreApi

    public constructor(
        configuration: Configuration,
        requestFactory?: StoreApiRequestFactory,
        responseProcessor?: StoreApiResponseProcessor
    ) {
        this.api = new ObservableStoreApi(configuration, requestFactory, responseProcessor);
    }

    /**
     * For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors
     * Delete purchase order by ID
     * @param orderId ID of the order that needs to be deleted
     */
    public deleteOrder(orderId: string, _options?: Configuration): Promise<void> {
        const result = this.api.deleteOrder(orderId, _options);
        return result.toPromise();
    }

    /**
     * Returns a map of status codes to quantities
     * Returns pet inventories by status
     */
    public getInventory(_options?: Configuration): Promise<{ [key: string]: number; }> {
        const result = this.api.getInventory(_options);
        return result.toPromise();
    }

    /**
     * For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions
     * Find purchase order by ID
     * @param orderId ID of pet that needs to be fetched
     */
    public getOrderById(orderId: number, _options?: Configuration): Promise<Order> {
        const result = this.api.getOrderById(orderId, _options);
        return result.toPromise();
    }

    /**
     * Place an order for a pet
     * @param order order placed for purchasing the pet
     */
    public placeOrder(order: Order, _options?: Configuration): Promise<Order> {
        const result = this.api.placeOrder(order, _options);
        return result.toPromise();
    }


}



import { ObservableUserApi } from './ObservableAPI';

import { UserApiRequestFactory, UserApiResponseProcessor} from "../apis/UserApi";
export class PromiseUserApi {
    private api: ObservableUserApi

    public constructor(
        configuration: Configuration,
        requestFactory?: UserApiRequestFactory,
        responseProcessor?: UserApiResponseProcessor
    ) {
        this.api = new ObservableUserApi(configuration, requestFactory, responseProcessor);
    }

    /**
     * This can only be done by the logged in user.
     * Create user
     * @param user Created user object
     */
    public createUser(user: User, _options?: Configuration): Promise<void> {
        const result = this.api.createUser(user, _options);
        return result.toPromise();
    }

    /**
     * Creates list of users with given input array
     * @param user List of user object
     */
    public createUsersWithArrayInput(user: Array<User>, _options?: Configuration): Promise<void> {
        const result = this.api.createUsersWithArrayInput(user, _options);
        return result.toPromise();
    }

    /**
     * Creates list of users with given input array
     * @param user List of user object
     */
    public createUsersWithListInput(user: Array<User>, _options?: Configuration): Promise<void> {
        const result = this.api.createUsersWithListInput(user, _options);
        return result.toPromise();
    }

    /**
     * This can only be done by the logged in user.
     * Delete user
     * @param username The name that needs to be deleted
     */
    public deleteUser(username: string, _options?: Configuration): Promise<void> {
        const result = this.api.deleteUser(username, _options);
        return result.toPromise();
    }

    /**
     * Get user by user name
     * @param username The name that needs to be fetched. Use user1 for testing.
     */
    public getUserByName(username: string, _options?: Configuration): Promise<User> {
        const result = this.api.getUserByName(username, _options);
        return result.toPromise();
    }

    /**
     * Logs user into the system
     * @param username The user name for login
     * @param password The password for login in clear text
     */
    public loginUser(username: string, password: string, _options?: Configuration): Promise<string> {
        const result = this.api.loginUser(username, password, _options);
        return result.toPromise();
    }

    /**
     * Logs out current logged in user session
     */
    public logoutUser(_options?: Configuration): Promise<void> {
        const result = this.api.logoutUser(_options);
        return result.toPromise();
    }

    /**
     * This can only be done by the logged in user.
     * Updated user
     * @param username name that need to be deleted
     * @param user Updated user object
     */
    public updateUser(username: string, user: User, _options?: Configuration): Promise<void> {
        const result = this.api.updateUser(username, user, _options);
        return result.toPromise();
    }


}



