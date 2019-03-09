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

	public constructor(configuration: Configuration) {
        this.api = new ObservablePetApi(configuration);
	}

    public addPet(pet: Pet, options?: Configuration): Promise<void> {
    	const result = this.api.addPet(pet, options);
        return result.toPromise();
    }
	
    public deletePet(petId: number, apiKey?: string, options?: Configuration): Promise<void> {
    	const result = this.api.deletePet(petId, apiKey, options);
        return result.toPromise();
    }
	
    public findPetsByStatus(status: Array<'available' | 'pending' | 'sold'>, options?: Configuration): Promise<Array<Pet>> {
    	const result = this.api.findPetsByStatus(status, options);
        return result.toPromise();
    }
	
    public findPetsByTags(tags: Array<string>, options?: Configuration): Promise<Array<Pet>> {
    	const result = this.api.findPetsByTags(tags, options);
        return result.toPromise();
    }
	
    public getPetById(petId: number, options?: Configuration): Promise<Pet> {
    	const result = this.api.getPetById(petId, options);
        return result.toPromise();
    }
	
    public updatePet(pet: Pet, options?: Configuration): Promise<void> {
    	const result = this.api.updatePet(pet, options);
        return result.toPromise();
    }
	
    public updatePetWithForm(petId: number, name?: string, status?: string, options?: Configuration): Promise<void> {
    	const result = this.api.updatePetWithForm(petId, name, status, options);
        return result.toPromise();
    }
	
    public uploadFile(petId: number, additionalMetadata?: string, file?: HttpFile, options?: Configuration): Promise<ApiResponse> {
    	const result = this.api.uploadFile(petId, additionalMetadata, file, options);
        return result.toPromise();
    }
	

}



import { ObservableStoreApi } from './ObservableAPI';


import { StoreApiRequestFactory, StoreApiResponseProcessor} from "../apis/StoreApi";
export class PromiseStoreApi {
    private api: ObservableStoreApi

	public constructor(configuration: Configuration) {
        this.api = new ObservableStoreApi(configuration);
	}

    public deleteOrder(orderId: string, options?: Configuration): Promise<void> {
    	const result = this.api.deleteOrder(orderId, options);
        return result.toPromise();
    }
	
    public getInventory(options?: Configuration): Promise<{ [key: string]: number; }> {
    	const result = this.api.getInventory(options);
        return result.toPromise();
    }
	
    public getOrderById(orderId: number, options?: Configuration): Promise<Order> {
    	const result = this.api.getOrderById(orderId, options);
        return result.toPromise();
    }
	
    public placeOrder(order: Order, options?: Configuration): Promise<Order> {
    	const result = this.api.placeOrder(order, options);
        return result.toPromise();
    }
	

}



import { ObservableUserApi } from './ObservableAPI';


import { UserApiRequestFactory, UserApiResponseProcessor} from "../apis/UserApi";
export class PromiseUserApi {
    private api: ObservableUserApi

	public constructor(configuration: Configuration) {
        this.api = new ObservableUserApi(configuration);
	}

    public createUser(user: User, options?: Configuration): Promise<void> {
    	const result = this.api.createUser(user, options);
        return result.toPromise();
    }
	
    public createUsersWithArrayInput(user: Array<User>, options?: Configuration): Promise<void> {
    	const result = this.api.createUsersWithArrayInput(user, options);
        return result.toPromise();
    }
	
    public createUsersWithListInput(user: Array<User>, options?: Configuration): Promise<void> {
    	const result = this.api.createUsersWithListInput(user, options);
        return result.toPromise();
    }
	
    public deleteUser(username: string, options?: Configuration): Promise<void> {
    	const result = this.api.deleteUser(username, options);
        return result.toPromise();
    }
	
    public getUserByName(username: string, options?: Configuration): Promise<User> {
    	const result = this.api.getUserByName(username, options);
        return result.toPromise();
    }
	
    public loginUser(username: string, password: string, options?: Configuration): Promise<string> {
    	const result = this.api.loginUser(username, password, options);
        return result.toPromise();
    }
	
    public logoutUser(options?: Configuration): Promise<void> {
    	const result = this.api.logoutUser(options);
        return result.toPromise();
    }
	
    public updateUser(username: string, user: User, options?: Configuration): Promise<void> {
    	const result = this.api.updateUser(username, user, options);
        return result.toPromise();
    }
	

}



