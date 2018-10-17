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

		// build promise chain
    	let middlewarePrePromise =Promise.resolve(requestContext);
    	for (let middleware of this.configuration.middleware) {
    		middlewarePrePromise = middlewarePrePromise.then((ctx) => middleware.pre(ctx));
    	}

    	return middlewarePrePromise.then((ctx) => this.configuration.httpApi.send(ctx)).
	    	then((response: ResponseContext) => {
	    		let middlewarePostPromise = Promise.resolve(response);
	    		for (let middleware of this.configuration.middleware) {
	    			middlewarePostPromise = middlewarePostPromise.then((rsp) => middleware.post(rsp));
	    		}
	    		return middlewarePostPromise.then((rsp) => this.responseProcessor.addPet(rsp));
	    	});
    }
	
    public deletePet(petId: number, apiKey?: string, options?: any): Promise<void> {
    	const requestContext = this.requestFactory.deletePet(petId, apiKey, options);

		// build promise chain
    	let middlewarePrePromise =Promise.resolve(requestContext);
    	for (let middleware of this.configuration.middleware) {
    		middlewarePrePromise = middlewarePrePromise.then((ctx) => middleware.pre(ctx));
    	}

    	return middlewarePrePromise.then((ctx) => this.configuration.httpApi.send(ctx)).
	    	then((response: ResponseContext) => {
	    		let middlewarePostPromise = Promise.resolve(response);
	    		for (let middleware of this.configuration.middleware) {
	    			middlewarePostPromise = middlewarePostPromise.then((rsp) => middleware.post(rsp));
	    		}
	    		return middlewarePostPromise.then((rsp) => this.responseProcessor.deletePet(rsp));
	    	});
    }
	
    public findPetsByStatus(status: Array<'available' | 'pending' | 'sold'>, options?: any): Promise<Array<Pet>> {
    	const requestContext = this.requestFactory.findPetsByStatus(status, options);

		// build promise chain
    	let middlewarePrePromise =Promise.resolve(requestContext);
    	for (let middleware of this.configuration.middleware) {
    		middlewarePrePromise = middlewarePrePromise.then((ctx) => middleware.pre(ctx));
    	}

    	return middlewarePrePromise.then((ctx) => this.configuration.httpApi.send(ctx)).
	    	then((response: ResponseContext) => {
	    		let middlewarePostPromise = Promise.resolve(response);
	    		for (let middleware of this.configuration.middleware) {
	    			middlewarePostPromise = middlewarePostPromise.then((rsp) => middleware.post(rsp));
	    		}
	    		return middlewarePostPromise.then((rsp) => this.responseProcessor.findPetsByStatus(rsp));
	    	});
    }
	
    public findPetsByTags(tags: Array<string>, options?: any): Promise<Array<Pet>> {
    	const requestContext = this.requestFactory.findPetsByTags(tags, options);

		// build promise chain
    	let middlewarePrePromise =Promise.resolve(requestContext);
    	for (let middleware of this.configuration.middleware) {
    		middlewarePrePromise = middlewarePrePromise.then((ctx) => middleware.pre(ctx));
    	}

    	return middlewarePrePromise.then((ctx) => this.configuration.httpApi.send(ctx)).
	    	then((response: ResponseContext) => {
	    		let middlewarePostPromise = Promise.resolve(response);
	    		for (let middleware of this.configuration.middleware) {
	    			middlewarePostPromise = middlewarePostPromise.then((rsp) => middleware.post(rsp));
	    		}
	    		return middlewarePostPromise.then((rsp) => this.responseProcessor.findPetsByTags(rsp));
	    	});
    }
	
    public getPetById(petId: number, options?: any): Promise<Pet> {
    	const requestContext = this.requestFactory.getPetById(petId, options);

		// build promise chain
    	let middlewarePrePromise =Promise.resolve(requestContext);
    	for (let middleware of this.configuration.middleware) {
    		middlewarePrePromise = middlewarePrePromise.then((ctx) => middleware.pre(ctx));
    	}

    	return middlewarePrePromise.then((ctx) => this.configuration.httpApi.send(ctx)).
	    	then((response: ResponseContext) => {
	    		let middlewarePostPromise = Promise.resolve(response);
	    		for (let middleware of this.configuration.middleware) {
	    			middlewarePostPromise = middlewarePostPromise.then((rsp) => middleware.post(rsp));
	    		}
	    		return middlewarePostPromise.then((rsp) => this.responseProcessor.getPetById(rsp));
	    	});
    }
	
    public updatePet(pet: Pet, options?: any): Promise<void> {
    	const requestContext = this.requestFactory.updatePet(pet, options);

		// build promise chain
    	let middlewarePrePromise =Promise.resolve(requestContext);
    	for (let middleware of this.configuration.middleware) {
    		middlewarePrePromise = middlewarePrePromise.then((ctx) => middleware.pre(ctx));
    	}

    	return middlewarePrePromise.then((ctx) => this.configuration.httpApi.send(ctx)).
	    	then((response: ResponseContext) => {
	    		let middlewarePostPromise = Promise.resolve(response);
	    		for (let middleware of this.configuration.middleware) {
	    			middlewarePostPromise = middlewarePostPromise.then((rsp) => middleware.post(rsp));
	    		}
	    		return middlewarePostPromise.then((rsp) => this.responseProcessor.updatePet(rsp));
	    	});
    }
	
    public updatePetWithForm(petId: number, name?: string, status?: string, options?: any): Promise<void> {
    	const requestContext = this.requestFactory.updatePetWithForm(petId, name, status, options);

		// build promise chain
    	let middlewarePrePromise =Promise.resolve(requestContext);
    	for (let middleware of this.configuration.middleware) {
    		middlewarePrePromise = middlewarePrePromise.then((ctx) => middleware.pre(ctx));
    	}

    	return middlewarePrePromise.then((ctx) => this.configuration.httpApi.send(ctx)).
	    	then((response: ResponseContext) => {
	    		let middlewarePostPromise = Promise.resolve(response);
	    		for (let middleware of this.configuration.middleware) {
	    			middlewarePostPromise = middlewarePostPromise.then((rsp) => middleware.post(rsp));
	    		}
	    		return middlewarePostPromise.then((rsp) => this.responseProcessor.updatePetWithForm(rsp));
	    	});
    }
	
    public uploadFile(petId: number, additionalMetadata?: string, file?: any, options?: any): Promise<ApiResponse> {
    	const requestContext = this.requestFactory.uploadFile(petId, additionalMetadata, file, options);

		// build promise chain
    	let middlewarePrePromise =Promise.resolve(requestContext);
    	for (let middleware of this.configuration.middleware) {
    		middlewarePrePromise = middlewarePrePromise.then((ctx) => middleware.pre(ctx));
    	}

    	return middlewarePrePromise.then((ctx) => this.configuration.httpApi.send(ctx)).
	    	then((response: ResponseContext) => {
	    		let middlewarePostPromise = Promise.resolve(response);
	    		for (let middleware of this.configuration.middleware) {
	    			middlewarePostPromise = middlewarePostPromise.then((rsp) => middleware.post(rsp));
	    		}
	    		return middlewarePostPromise.then((rsp) => this.responseProcessor.uploadFile(rsp));
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

		// build promise chain
    	let middlewarePrePromise =Promise.resolve(requestContext);
    	for (let middleware of this.configuration.middleware) {
    		middlewarePrePromise = middlewarePrePromise.then((ctx) => middleware.pre(ctx));
    	}

    	return middlewarePrePromise.then((ctx) => this.configuration.httpApi.send(ctx)).
	    	then((response: ResponseContext) => {
	    		let middlewarePostPromise = Promise.resolve(response);
	    		for (let middleware of this.configuration.middleware) {
	    			middlewarePostPromise = middlewarePostPromise.then((rsp) => middleware.post(rsp));
	    		}
	    		return middlewarePostPromise.then((rsp) => this.responseProcessor.deleteOrder(rsp));
	    	});
    }
	
    public getInventory(options?: any): Promise<{ [key: string]: number; }> {
    	const requestContext = this.requestFactory.getInventory(options);

		// build promise chain
    	let middlewarePrePromise =Promise.resolve(requestContext);
    	for (let middleware of this.configuration.middleware) {
    		middlewarePrePromise = middlewarePrePromise.then((ctx) => middleware.pre(ctx));
    	}

    	return middlewarePrePromise.then((ctx) => this.configuration.httpApi.send(ctx)).
	    	then((response: ResponseContext) => {
	    		let middlewarePostPromise = Promise.resolve(response);
	    		for (let middleware of this.configuration.middleware) {
	    			middlewarePostPromise = middlewarePostPromise.then((rsp) => middleware.post(rsp));
	    		}
	    		return middlewarePostPromise.then((rsp) => this.responseProcessor.getInventory(rsp));
	    	});
    }
	
    public getOrderById(orderId: number, options?: any): Promise<Order> {
    	const requestContext = this.requestFactory.getOrderById(orderId, options);

		// build promise chain
    	let middlewarePrePromise =Promise.resolve(requestContext);
    	for (let middleware of this.configuration.middleware) {
    		middlewarePrePromise = middlewarePrePromise.then((ctx) => middleware.pre(ctx));
    	}

    	return middlewarePrePromise.then((ctx) => this.configuration.httpApi.send(ctx)).
	    	then((response: ResponseContext) => {
	    		let middlewarePostPromise = Promise.resolve(response);
	    		for (let middleware of this.configuration.middleware) {
	    			middlewarePostPromise = middlewarePostPromise.then((rsp) => middleware.post(rsp));
	    		}
	    		return middlewarePostPromise.then((rsp) => this.responseProcessor.getOrderById(rsp));
	    	});
    }
	
    public placeOrder(order: Order, options?: any): Promise<Order> {
    	const requestContext = this.requestFactory.placeOrder(order, options);

		// build promise chain
    	let middlewarePrePromise =Promise.resolve(requestContext);
    	for (let middleware of this.configuration.middleware) {
    		middlewarePrePromise = middlewarePrePromise.then((ctx) => middleware.pre(ctx));
    	}

    	return middlewarePrePromise.then((ctx) => this.configuration.httpApi.send(ctx)).
	    	then((response: ResponseContext) => {
	    		let middlewarePostPromise = Promise.resolve(response);
	    		for (let middleware of this.configuration.middleware) {
	    			middlewarePostPromise = middlewarePostPromise.then((rsp) => middleware.post(rsp));
	    		}
	    		return middlewarePostPromise.then((rsp) => this.responseProcessor.placeOrder(rsp));
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

		// build promise chain
    	let middlewarePrePromise =Promise.resolve(requestContext);
    	for (let middleware of this.configuration.middleware) {
    		middlewarePrePromise = middlewarePrePromise.then((ctx) => middleware.pre(ctx));
    	}

    	return middlewarePrePromise.then((ctx) => this.configuration.httpApi.send(ctx)).
	    	then((response: ResponseContext) => {
	    		let middlewarePostPromise = Promise.resolve(response);
	    		for (let middleware of this.configuration.middleware) {
	    			middlewarePostPromise = middlewarePostPromise.then((rsp) => middleware.post(rsp));
	    		}
	    		return middlewarePostPromise.then((rsp) => this.responseProcessor.createUser(rsp));
	    	});
    }
	
    public createUsersWithArrayInput(user: Array<User>, options?: any): Promise<void> {
    	const requestContext = this.requestFactory.createUsersWithArrayInput(user, options);

		// build promise chain
    	let middlewarePrePromise =Promise.resolve(requestContext);
    	for (let middleware of this.configuration.middleware) {
    		middlewarePrePromise = middlewarePrePromise.then((ctx) => middleware.pre(ctx));
    	}

    	return middlewarePrePromise.then((ctx) => this.configuration.httpApi.send(ctx)).
	    	then((response: ResponseContext) => {
	    		let middlewarePostPromise = Promise.resolve(response);
	    		for (let middleware of this.configuration.middleware) {
	    			middlewarePostPromise = middlewarePostPromise.then((rsp) => middleware.post(rsp));
	    		}
	    		return middlewarePostPromise.then((rsp) => this.responseProcessor.createUsersWithArrayInput(rsp));
	    	});
    }
	
    public createUsersWithListInput(user: Array<User>, options?: any): Promise<void> {
    	const requestContext = this.requestFactory.createUsersWithListInput(user, options);

		// build promise chain
    	let middlewarePrePromise =Promise.resolve(requestContext);
    	for (let middleware of this.configuration.middleware) {
    		middlewarePrePromise = middlewarePrePromise.then((ctx) => middleware.pre(ctx));
    	}

    	return middlewarePrePromise.then((ctx) => this.configuration.httpApi.send(ctx)).
	    	then((response: ResponseContext) => {
	    		let middlewarePostPromise = Promise.resolve(response);
	    		for (let middleware of this.configuration.middleware) {
	    			middlewarePostPromise = middlewarePostPromise.then((rsp) => middleware.post(rsp));
	    		}
	    		return middlewarePostPromise.then((rsp) => this.responseProcessor.createUsersWithListInput(rsp));
	    	});
    }
	
    public deleteUser(username: string, options?: any): Promise<void> {
    	const requestContext = this.requestFactory.deleteUser(username, options);

		// build promise chain
    	let middlewarePrePromise =Promise.resolve(requestContext);
    	for (let middleware of this.configuration.middleware) {
    		middlewarePrePromise = middlewarePrePromise.then((ctx) => middleware.pre(ctx));
    	}

    	return middlewarePrePromise.then((ctx) => this.configuration.httpApi.send(ctx)).
	    	then((response: ResponseContext) => {
	    		let middlewarePostPromise = Promise.resolve(response);
	    		for (let middleware of this.configuration.middleware) {
	    			middlewarePostPromise = middlewarePostPromise.then((rsp) => middleware.post(rsp));
	    		}
	    		return middlewarePostPromise.then((rsp) => this.responseProcessor.deleteUser(rsp));
	    	});
    }
	
    public getUserByName(username: string, options?: any): Promise<User> {
    	const requestContext = this.requestFactory.getUserByName(username, options);

		// build promise chain
    	let middlewarePrePromise =Promise.resolve(requestContext);
    	for (let middleware of this.configuration.middleware) {
    		middlewarePrePromise = middlewarePrePromise.then((ctx) => middleware.pre(ctx));
    	}

    	return middlewarePrePromise.then((ctx) => this.configuration.httpApi.send(ctx)).
	    	then((response: ResponseContext) => {
	    		let middlewarePostPromise = Promise.resolve(response);
	    		for (let middleware of this.configuration.middleware) {
	    			middlewarePostPromise = middlewarePostPromise.then((rsp) => middleware.post(rsp));
	    		}
	    		return middlewarePostPromise.then((rsp) => this.responseProcessor.getUserByName(rsp));
	    	});
    }
	
    public loginUser(username: string, password: string, options?: any): Promise<string> {
    	const requestContext = this.requestFactory.loginUser(username, password, options);

		// build promise chain
    	let middlewarePrePromise =Promise.resolve(requestContext);
    	for (let middleware of this.configuration.middleware) {
    		middlewarePrePromise = middlewarePrePromise.then((ctx) => middleware.pre(ctx));
    	}

    	return middlewarePrePromise.then((ctx) => this.configuration.httpApi.send(ctx)).
	    	then((response: ResponseContext) => {
	    		let middlewarePostPromise = Promise.resolve(response);
	    		for (let middleware of this.configuration.middleware) {
	    			middlewarePostPromise = middlewarePostPromise.then((rsp) => middleware.post(rsp));
	    		}
	    		return middlewarePostPromise.then((rsp) => this.responseProcessor.loginUser(rsp));
	    	});
    }
	
    public logoutUser(options?: any): Promise<void> {
    	const requestContext = this.requestFactory.logoutUser(options);

		// build promise chain
    	let middlewarePrePromise =Promise.resolve(requestContext);
    	for (let middleware of this.configuration.middleware) {
    		middlewarePrePromise = middlewarePrePromise.then((ctx) => middleware.pre(ctx));
    	}

    	return middlewarePrePromise.then((ctx) => this.configuration.httpApi.send(ctx)).
	    	then((response: ResponseContext) => {
	    		let middlewarePostPromise = Promise.resolve(response);
	    		for (let middleware of this.configuration.middleware) {
	    			middlewarePostPromise = middlewarePostPromise.then((rsp) => middleware.post(rsp));
	    		}
	    		return middlewarePostPromise.then((rsp) => this.responseProcessor.logoutUser(rsp));
	    	});
    }
	
    public updateUser(username: string, user: User, options?: any): Promise<void> {
    	const requestContext = this.requestFactory.updateUser(username, user, options);

		// build promise chain
    	let middlewarePrePromise =Promise.resolve(requestContext);
    	for (let middleware of this.configuration.middleware) {
    		middlewarePrePromise = middlewarePrePromise.then((ctx) => middleware.pre(ctx));
    	}

    	return middlewarePrePromise.then((ctx) => this.configuration.httpApi.send(ctx)).
	    	then((response: ResponseContext) => {
	    		let middlewarePostPromise = Promise.resolve(response);
	    		for (let middleware of this.configuration.middleware) {
	    			middlewarePostPromise = middlewarePostPromise.then((rsp) => middleware.post(rsp));
	    		}
	    		return middlewarePostPromise.then((rsp) => this.responseProcessor.updateUser(rsp));
	    	});
    }
	

}



