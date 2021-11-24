import { ResponseContext, RequestContext, HttpFile } from '../http/http.ts';
import * as models from '../models/all.ts';
import { Configuration} from '../configuration.ts'

import { ApiResponse } from '../models/ApiResponse.ts';
import { Category } from '../models/Category.ts';
import { Order } from '../models/Order.ts';
import { Pet } from '../models/Pet.ts';
import { Tag } from '../models/Tag.ts';
import { User } from '../models/User.ts';

import { ObservablePetApi } from "./ObservableAPI.ts";
import { PetApiRequestFactory, PetApiResponseProcessor} from "../apis/PetApi.ts";

export interface PetApiAddPetRequest {
    /**
     * Pet object that needs to be added to the store
     * @type Pet
     * @memberof PetApiaddPet
     */
    pet: Pet
}

export interface PetApiDeletePetRequest {
    /**
     * Pet id to delete
     * @type number
     * @memberof PetApideletePet
     */
    petId: number
    /**
     * 
     * @type string
     * @memberof PetApideletePet
     */
    apiKey?: string
}

export interface PetApiFindPetsByStatusRequest {
    /**
     * Status values that need to be considered for filter
     * @type Array&lt;&#39;available&#39; | &#39;pending&#39; | &#39;sold&#39;&gt;
     * @memberof PetApifindPetsByStatus
     */
    status: Array<'available' | 'pending' | 'sold'>
}

export interface PetApiFindPetsByTagsRequest {
    /**
     * Tags to filter by
     * @type Array&lt;string&gt;
     * @memberof PetApifindPetsByTags
     */
    tags: Array<string>
}

export interface PetApiGetPetByIdRequest {
    /**
     * ID of pet to return
     * @type number
     * @memberof PetApigetPetById
     */
    petId: number
}

export interface PetApiUpdatePetRequest {
    /**
     * Pet object that needs to be added to the store
     * @type Pet
     * @memberof PetApiupdatePet
     */
    pet: Pet
}

export interface PetApiUpdatePetWithFormRequest {
    /**
     * ID of pet that needs to be updated
     * @type number
     * @memberof PetApiupdatePetWithForm
     */
    petId: number
    /**
     * Updated name of the pet
     * @type string
     * @memberof PetApiupdatePetWithForm
     */
    name?: string
    /**
     * Updated status of the pet
     * @type string
     * @memberof PetApiupdatePetWithForm
     */
    status?: string
}

export interface PetApiUploadFileRequest {
    /**
     * ID of pet to update
     * @type number
     * @memberof PetApiuploadFile
     */
    petId: number
    /**
     * Additional data to pass to server
     * @type string
     * @memberof PetApiuploadFile
     */
    additionalMetadata?: string
    /**
     * file to upload
     * @type HttpFile
     * @memberof PetApiuploadFile
     */
    file?: HttpFile
}

export class ObjectPetApi {
    private api: ObservablePetApi

    public constructor(configuration: Configuration, requestFactory?: PetApiRequestFactory, responseProcessor?: PetApiResponseProcessor) {
        this.api = new ObservablePetApi(configuration, requestFactory, responseProcessor);
    }

    /**
     * Add a new pet to the store
     * @param param the request object
     */
    public addPet(param: PetApiAddPetRequest, options?: Configuration): Promise<Pet> {
        return this.api.addPet(param.pet,  options).toPromise();
    }

    /**
     * Deletes a pet
     * @param param the request object
     */
    public deletePet(param: PetApiDeletePetRequest, options?: Configuration): Promise<void> {
        return this.api.deletePet(param.petId, param.apiKey,  options).toPromise();
    }

    /**
     * Multiple status values can be provided with comma separated strings
     * Finds Pets by status
     * @param param the request object
     */
    public findPetsByStatus(param: PetApiFindPetsByStatusRequest, options?: Configuration): Promise<Array<Pet>> {
        return this.api.findPetsByStatus(param.status,  options).toPromise();
    }

    /**
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     * Finds Pets by tags
     * @param param the request object
     */
    public findPetsByTags(param: PetApiFindPetsByTagsRequest, options?: Configuration): Promise<Array<Pet>> {
        return this.api.findPetsByTags(param.tags,  options).toPromise();
    }

    /**
     * Returns a single pet
     * Find pet by ID
     * @param param the request object
     */
    public getPetById(param: PetApiGetPetByIdRequest, options?: Configuration): Promise<Pet> {
        return this.api.getPetById(param.petId,  options).toPromise();
    }

    /**
     * Update an existing pet
     * @param param the request object
     */
    public updatePet(param: PetApiUpdatePetRequest, options?: Configuration): Promise<Pet> {
        return this.api.updatePet(param.pet,  options).toPromise();
    }

    /**
     * Updates a pet in the store with form data
     * @param param the request object
     */
    public updatePetWithForm(param: PetApiUpdatePetWithFormRequest, options?: Configuration): Promise<void> {
        return this.api.updatePetWithForm(param.petId, param.name, param.status,  options).toPromise();
    }

    /**
     * uploads an image
     * @param param the request object
     */
    public uploadFile(param: PetApiUploadFileRequest, options?: Configuration): Promise<ApiResponse> {
        return this.api.uploadFile(param.petId, param.additionalMetadata, param.file,  options).toPromise();
    }

}

import { ObservableStoreApi } from "./ObservableAPI.ts";
import { StoreApiRequestFactory, StoreApiResponseProcessor} from "../apis/StoreApi.ts";

export interface StoreApiDeleteOrderRequest {
    /**
     * ID of the order that needs to be deleted
     * @type string
     * @memberof StoreApideleteOrder
     */
    orderId: string
}

export interface StoreApiGetInventoryRequest {
}

export interface StoreApiGetOrderByIdRequest {
    /**
     * ID of pet that needs to be fetched
     * @type number
     * @memberof StoreApigetOrderById
     */
    orderId: number
}

export interface StoreApiPlaceOrderRequest {
    /**
     * order placed for purchasing the pet
     * @type Order
     * @memberof StoreApiplaceOrder
     */
    order: Order
}

export class ObjectStoreApi {
    private api: ObservableStoreApi

    public constructor(configuration: Configuration, requestFactory?: StoreApiRequestFactory, responseProcessor?: StoreApiResponseProcessor) {
        this.api = new ObservableStoreApi(configuration, requestFactory, responseProcessor);
    }

    /**
     * For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors
     * Delete purchase order by ID
     * @param param the request object
     */
    public deleteOrder(param: StoreApiDeleteOrderRequest, options?: Configuration): Promise<void> {
        return this.api.deleteOrder(param.orderId,  options).toPromise();
    }

    /**
     * Returns a map of status codes to quantities
     * Returns pet inventories by status
     * @param param the request object
     */
    public getInventory(param: StoreApiGetInventoryRequest, options?: Configuration): Promise<{ [key: string]: number; }> {
        return this.api.getInventory( options).toPromise();
    }

    /**
     * For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions
     * Find purchase order by ID
     * @param param the request object
     */
    public getOrderById(param: StoreApiGetOrderByIdRequest, options?: Configuration): Promise<Order> {
        return this.api.getOrderById(param.orderId,  options).toPromise();
    }

    /**
     * Place an order for a pet
     * @param param the request object
     */
    public placeOrder(param: StoreApiPlaceOrderRequest, options?: Configuration): Promise<Order> {
        return this.api.placeOrder(param.order,  options).toPromise();
    }

}

import { ObservableUserApi } from "./ObservableAPI.ts";
import { UserApiRequestFactory, UserApiResponseProcessor} from "../apis/UserApi.ts";

export interface UserApiCreateUserRequest {
    /**
     * Created user object
     * @type User
     * @memberof UserApicreateUser
     */
    user: User
}

export interface UserApiCreateUsersWithArrayInputRequest {
    /**
     * List of user object
     * @type Array&lt;User&gt;
     * @memberof UserApicreateUsersWithArrayInput
     */
    user: Array<User>
}

export interface UserApiCreateUsersWithListInputRequest {
    /**
     * List of user object
     * @type Array&lt;User&gt;
     * @memberof UserApicreateUsersWithListInput
     */
    user: Array<User>
}

export interface UserApiDeleteUserRequest {
    /**
     * The name that needs to be deleted
     * @type string
     * @memberof UserApideleteUser
     */
    username: string
}

export interface UserApiGetUserByNameRequest {
    /**
     * The name that needs to be fetched. Use user1 for testing.
     * @type string
     * @memberof UserApigetUserByName
     */
    username: string
}

export interface UserApiLoginUserRequest {
    /**
     * The user name for login
     * @type string
     * @memberof UserApiloginUser
     */
    username: string
    /**
     * The password for login in clear text
     * @type string
     * @memberof UserApiloginUser
     */
    password: string
}

export interface UserApiLogoutUserRequest {
}

export interface UserApiUpdateUserRequest {
    /**
     * name that need to be deleted
     * @type string
     * @memberof UserApiupdateUser
     */
    username: string
    /**
     * Updated user object
     * @type User
     * @memberof UserApiupdateUser
     */
    user: User
}

export class ObjectUserApi {
    private api: ObservableUserApi

    public constructor(configuration: Configuration, requestFactory?: UserApiRequestFactory, responseProcessor?: UserApiResponseProcessor) {
        this.api = new ObservableUserApi(configuration, requestFactory, responseProcessor);
    }

    /**
     * This can only be done by the logged in user.
     * Create user
     * @param param the request object
     */
    public createUser(param: UserApiCreateUserRequest, options?: Configuration): Promise<void> {
        return this.api.createUser(param.user,  options).toPromise();
    }

    /**
     * Creates list of users with given input array
     * @param param the request object
     */
    public createUsersWithArrayInput(param: UserApiCreateUsersWithArrayInputRequest, options?: Configuration): Promise<void> {
        return this.api.createUsersWithArrayInput(param.user,  options).toPromise();
    }

    /**
     * Creates list of users with given input array
     * @param param the request object
     */
    public createUsersWithListInput(param: UserApiCreateUsersWithListInputRequest, options?: Configuration): Promise<void> {
        return this.api.createUsersWithListInput(param.user,  options).toPromise();
    }

    /**
     * This can only be done by the logged in user.
     * Delete user
     * @param param the request object
     */
    public deleteUser(param: UserApiDeleteUserRequest, options?: Configuration): Promise<void> {
        return this.api.deleteUser(param.username,  options).toPromise();
    }

    /**
     * Get user by user name
     * @param param the request object
     */
    public getUserByName(param: UserApiGetUserByNameRequest, options?: Configuration): Promise<User> {
        return this.api.getUserByName(param.username,  options).toPromise();
    }

    /**
     * Logs user into the system
     * @param param the request object
     */
    public loginUser(param: UserApiLoginUserRequest, options?: Configuration): Promise<string> {
        return this.api.loginUser(param.username, param.password,  options).toPromise();
    }

    /**
     * Logs out current logged in user session
     * @param param the request object
     */
    public logoutUser(param: UserApiLogoutUserRequest, options?: Configuration): Promise<void> {
        return this.api.logoutUser( options).toPromise();
    }

    /**
     * This can only be done by the logged in user.
     * Updated user
     * @param param the request object
     */
    public updateUser(param: UserApiUpdateUserRequest, options?: Configuration): Promise<void> {
        return this.api.updateUser(param.username, param.user,  options).toPromise();
    }

}
