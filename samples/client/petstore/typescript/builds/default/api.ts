import { ResponseContext } from './http/http';
import * as models from './models/all';
import { Configuration} from './configuration'

import { ApiResponse } from './models/ApiResponse';
import { Category } from './models/Category';
import { Order } from './models/Order';
import { Pet } from './models/Pet';
import { Tag } from './models/Tag';
import { User } from './models/User';

import { PetApiRequestFactory, PetApiResponseProcessor} from "./apis/PetApi";
export class PetApi {
	private requestFactory: PetApiRequestFactory;
	private responseProcessor: PetApiResponseProcessor;

	public constructor(private configuration: Configuration) {
		this.requestFactory = new PetApiRequestFactory(configuration);
		this.responseProcessor = new PetApiResponseProcessor();
	}

    public addPet(pet: Pet, options?: any): Promise<void> {
    	const requestContext = this.requestFactory.addPet(pet, options);
    	
    	return this.configuration.httpApi.send(requestContext).then((response: ResponseContext) => {
    		return this.responseProcessor.addPet(response);
    	});
    }
	
    public deletePet(petId: number, apiKey?: string, options?: any): Promise<void> {
    	const requestContext = this.requestFactory.deletePet(petId, apiKey, options);
    	
    	return this.configuration.httpApi.send(requestContext).then((response: ResponseContext) => {
    		return this.responseProcessor.deletePet(response);
    	});
    }
	
    public findPetsByStatus(status: Array<'available' | 'pending' | 'sold'>, options?: any): Promise<Array<Pet>> {
    	const requestContext = this.requestFactory.findPetsByStatus(status, options);
    	
    	return this.configuration.httpApi.send(requestContext).then((response: ResponseContext) => {
    		return this.responseProcessor.findPetsByStatus(response);
    	});
    }
	
    public findPetsByTags(tags: Array<string>, options?: any): Promise<Array<Pet>> {
    	const requestContext = this.requestFactory.findPetsByTags(tags, options);
    	
    	return this.configuration.httpApi.send(requestContext).then((response: ResponseContext) => {
    		return this.responseProcessor.findPetsByTags(response);
    	});
    }
	
    public getPetById(petId: number, options?: any): Promise<Pet> {
    	const requestContext = this.requestFactory.getPetById(petId, options);
    	
    	return this.configuration.httpApi.send(requestContext).then((response: ResponseContext) => {
    		return this.responseProcessor.getPetById(response);
    	});
    }
	
    public updatePet(pet: Pet, options?: any): Promise<void> {
    	const requestContext = this.requestFactory.updatePet(pet, options);
    	
    	return this.configuration.httpApi.send(requestContext).then((response: ResponseContext) => {
    		return this.responseProcessor.updatePet(response);
    	});
    }
	
    public updatePetWithForm(petId: number, name?: string, status?: string, options?: any): Promise<void> {
    	const requestContext = this.requestFactory.updatePetWithForm(petId, name, status, options);
    	
    	return this.configuration.httpApi.send(requestContext).then((response: ResponseContext) => {
    		return this.responseProcessor.updatePetWithForm(response);
    	});
    }
	
    public uploadFile(petId: number, additionalMetadata?: string, file?: any, options?: any): Promise<ApiResponse> {
    	const requestContext = this.requestFactory.uploadFile(petId, additionalMetadata, file, options);
    	
    	return this.configuration.httpApi.send(requestContext).then((response: ResponseContext) => {
    		return this.responseProcessor.uploadFile(response);
    	});
    }
	

}




import { StoreApiRequestFactory, StoreApiResponseProcessor} from "./apis/StoreApi";
export class StoreApi {
	private requestFactory: StoreApiRequestFactory;
	private responseProcessor: StoreApiResponseProcessor;

	public constructor(private configuration: Configuration) {
		this.requestFactory = new StoreApiRequestFactory(configuration);
		this.responseProcessor = new StoreApiResponseProcessor();
	}

    public deleteOrder(orderId: string, options?: any): Promise<void> {
    	const requestContext = this.requestFactory.deleteOrder(orderId, options);
    	
    	return this.configuration.httpApi.send(requestContext).then((response: ResponseContext) => {
    		return this.responseProcessor.deleteOrder(response);
    	});
    }
	
    public getInventory(options?: any): Promise<{ [key: string]: number; }> {
    	const requestContext = this.requestFactory.getInventory(options);
    	
    	return this.configuration.httpApi.send(requestContext).then((response: ResponseContext) => {
    		return this.responseProcessor.getInventory(response);
    	});
    }
	
    public getOrderById(orderId: number, options?: any): Promise<Order> {
    	const requestContext = this.requestFactory.getOrderById(orderId, options);
    	
    	return this.configuration.httpApi.send(requestContext).then((response: ResponseContext) => {
    		return this.responseProcessor.getOrderById(response);
    	});
    }
	
    public placeOrder(order: Order, options?: any): Promise<Order> {
    	const requestContext = this.requestFactory.placeOrder(order, options);
    	
    	return this.configuration.httpApi.send(requestContext).then((response: ResponseContext) => {
    		return this.responseProcessor.placeOrder(response);
    	});
    }
	

}




import { UserApiRequestFactory, UserApiResponseProcessor} from "./apis/UserApi";
export class UserApi {
	private requestFactory: UserApiRequestFactory;
	private responseProcessor: UserApiResponseProcessor;

	public constructor(private configuration: Configuration) {
		this.requestFactory = new UserApiRequestFactory(configuration);
		this.responseProcessor = new UserApiResponseProcessor();
	}

    public createUser(user: User, options?: any): Promise<void> {
    	const requestContext = this.requestFactory.createUser(user, options);
    	
    	return this.configuration.httpApi.send(requestContext).then((response: ResponseContext) => {
    		return this.responseProcessor.createUser(response);
    	});
    }
	
    public createUsersWithArrayInput(user: Array<User>, options?: any): Promise<void> {
    	const requestContext = this.requestFactory.createUsersWithArrayInput(user, options);
    	
    	return this.configuration.httpApi.send(requestContext).then((response: ResponseContext) => {
    		return this.responseProcessor.createUsersWithArrayInput(response);
    	});
    }
	
    public createUsersWithListInput(user: Array<User>, options?: any): Promise<void> {
    	const requestContext = this.requestFactory.createUsersWithListInput(user, options);
    	
    	return this.configuration.httpApi.send(requestContext).then((response: ResponseContext) => {
    		return this.responseProcessor.createUsersWithListInput(response);
    	});
    }
	
    public deleteUser(username: string, options?: any): Promise<void> {
    	const requestContext = this.requestFactory.deleteUser(username, options);
    	
    	return this.configuration.httpApi.send(requestContext).then((response: ResponseContext) => {
    		return this.responseProcessor.deleteUser(response);
    	});
    }
	
    public getUserByName(username: string, options?: any): Promise<User> {
    	const requestContext = this.requestFactory.getUserByName(username, options);
    	
    	return this.configuration.httpApi.send(requestContext).then((response: ResponseContext) => {
    		return this.responseProcessor.getUserByName(response);
    	});
    }
	
    public loginUser(username: string, password: string, options?: any): Promise<string> {
    	const requestContext = this.requestFactory.loginUser(username, password, options);
    	
    	return this.configuration.httpApi.send(requestContext).then((response: ResponseContext) => {
    		return this.responseProcessor.loginUser(response);
    	});
    }
	
    public logoutUser(options?: any): Promise<void> {
    	const requestContext = this.requestFactory.logoutUser(options);
    	
    	return this.configuration.httpApi.send(requestContext).then((response: ResponseContext) => {
    		return this.responseProcessor.logoutUser(response);
    	});
    }
	
    public updateUser(username: string, user: User, options?: any): Promise<void> {
    	const requestContext = this.requestFactory.updateUser(username, user, options);
    	
    	return this.configuration.httpApi.send(requestContext).then((response: ResponseContext) => {
    		return this.responseProcessor.updateUser(response);
    	});
    }
	

}



