import type { HttpFile } from '../http/http';
import type { Configuration } from '../configuration'
import type * as req from "../types/ObjectParamAPI";

import type { ApiResponse } from '../models/ApiResponse';
import type { Category } from '../models/Category';
import type { Order } from '../models/Order';
import type { Pet } from '../models/Pet';
import type { Tag } from '../models/Tag';
import type { User } from '../models/User';


export abstract class AbstractObjectPetApi {
    /**
     * Add a new pet to the store
     * @param param the request object
     */
    public abstract addPet(param: req.PetApiAddPetRequest, options?: Configuration): Promise<Pet>;

    /**
     * Deletes a pet
     * @param param the request object
     */
    public abstract deletePet(param: req.PetApiDeletePetRequest, options?: Configuration): Promise<void>;

    /**
     * Multiple status values can be provided with comma separated strings
     * Finds Pets by status
     * @param param the request object
     */
    public abstract findPetsByStatus(param: req.PetApiFindPetsByStatusRequest, options?: Configuration): Promise<Array<Pet>>;

    /**
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     * Finds Pets by tags
     * @param param the request object
     */
    public abstract findPetsByTags(param: req.PetApiFindPetsByTagsRequest, options?: Configuration): Promise<Array<Pet>>;

    /**
     * Returns a single pet
     * Find pet by ID
     * @param param the request object
     */
    public abstract getPetById(param: req.PetApiGetPetByIdRequest, options?: Configuration): Promise<Pet>;

    /**
     * Update an existing pet
     * @param param the request object
     */
    public abstract updatePet(param: req.PetApiUpdatePetRequest, options?: Configuration): Promise<Pet>;

    /**
     * Updates a pet in the store with form data
     * @param param the request object
     */
    public abstract updatePetWithForm(param: req.PetApiUpdatePetWithFormRequest, options?: Configuration): Promise<void>;

    /**
     * uploads an image
     * @param param the request object
     */
    public abstract uploadFile(param: req.PetApiUploadFileRequest, options?: Configuration): Promise<ApiResponse>;

}


export abstract class AbstractObjectStoreApi {
    /**
     * For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors
     * Delete purchase order by ID
     * @param param the request object
     */
    public abstract deleteOrder(param: req.StoreApiDeleteOrderRequest, options?: Configuration): Promise<void>;

    /**
     * Returns a map of status codes to quantities
     * Returns pet inventories by status
     * @param param the request object
     */
    public abstract getInventory(param: req.StoreApiGetInventoryRequest, options?: Configuration): Promise<{ [key: string]: number; }>;

    /**
     * For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions
     * Find purchase order by ID
     * @param param the request object
     */
    public abstract getOrderById(param: req.StoreApiGetOrderByIdRequest, options?: Configuration): Promise<Order>;

    /**
     * Place an order for a pet
     * @param param the request object
     */
    public abstract placeOrder(param: req.StoreApiPlaceOrderRequest, options?: Configuration): Promise<Order>;

}


export abstract class AbstractObjectUserApi {
    /**
     * This can only be done by the logged in user.
     * Create user
     * @param param the request object
     */
    public abstract createUser(param: req.UserApiCreateUserRequest, options?: Configuration): Promise<void>;

    /**
     * Creates list of users with given input array
     * @param param the request object
     */
    public abstract createUsersWithArrayInput(param: req.UserApiCreateUsersWithArrayInputRequest, options?: Configuration): Promise<void>;

    /**
     * Creates list of users with given input array
     * @param param the request object
     */
    public abstract createUsersWithListInput(param: req.UserApiCreateUsersWithListInputRequest, options?: Configuration): Promise<void>;

    /**
     * This can only be done by the logged in user.
     * Delete user
     * @param param the request object
     */
    public abstract deleteUser(param: req.UserApiDeleteUserRequest, options?: Configuration): Promise<void>;

    /**
     * Get user by user name
     * @param param the request object
     */
    public abstract getUserByName(param: req.UserApiGetUserByNameRequest, options?: Configuration): Promise<User>;

    /**
     * Logs user into the system
     * @param param the request object
     */
    public abstract loginUser(param: req.UserApiLoginUserRequest, options?: Configuration): Promise<string>;

    /**
     * Logs out current logged in user session
     * @param param the request object
     */
    public abstract logoutUser(param: req.UserApiLogoutUserRequest, options?: Configuration): Promise<void>;

    /**
     * This can only be done by the logged in user.
     * Updated user
     * @param param the request object
     */
    public abstract updateUser(param: req.UserApiUpdateUserRequest, options?: Configuration): Promise<void>;

}
