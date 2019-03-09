import { ResponseContext, RequestContext, HttpFile } from '../http/http';
import * as models from '../models/all';
import { Configuration} from '../configuration'
import { Observable, of } from 'rxjs';
import {mergeMap, map} from  'rxjs/operators';

import { ApiResponse } from '../models/ApiResponse';
import { Category } from '../models/Category';
import { Order } from '../models/Order';
import { Pet } from '../models/Pet';
import { Tag } from '../models/Tag';
import { User } from '../models/User';

import { PetApiRequestFactory, PetApiResponseProcessor} from "../apis/PetApi";
export class ObservablePetApi {
	private requestFactory: PetApiRequestFactory;
	private responseProcessor: PetApiResponseProcessor;
    private configuration: Configuration;
    
	public constructor(configuration: Configuration, requestFactory?: PetApiRequestFactory, responseProcessor?: PetApiResponseProcessor) {
	    this.configuration = configuration;
		this.requestFactory = requestFactory || new PetApiRequestFactory(configuration);
		this.responseProcessor = responseProcessor || new PetApiResponseProcessor();
	}

    public addPet(pet: Pet, options?: Configuration): Observable<void> {
    	const requestContext = this.requestFactory.addPet(pet, options);

		// build promise chain
    	let middlewarePreObservable = of(requestContext);
    	for (let middleware of this.configuration.middleware) {
    		middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
    	}

    	return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => this.configuration.httpApi.send(ctx))).
	    	pipe(mergeMap((response: ResponseContext) => {
	    		let middlewarePostObservable = of(response);
	    		for (let middleware of this.configuration.middleware) {
	    			middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
	    		}
	    		return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.addPet(rsp)));
	    	}));
    }
	
    public deletePet(petId: number, apiKey?: string, options?: Configuration): Observable<void> {
    	const requestContext = this.requestFactory.deletePet(petId, apiKey, options);

		// build promise chain
    	let middlewarePreObservable = of(requestContext);
    	for (let middleware of this.configuration.middleware) {
    		middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
    	}

    	return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => this.configuration.httpApi.send(ctx))).
	    	pipe(mergeMap((response: ResponseContext) => {
	    		let middlewarePostObservable = of(response);
	    		for (let middleware of this.configuration.middleware) {
	    			middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
	    		}
	    		return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.deletePet(rsp)));
	    	}));
    }
	
    public findPetsByStatus(status: Array<'available' | 'pending' | 'sold'>, options?: Configuration): Observable<Array<Pet>> {
    	const requestContext = this.requestFactory.findPetsByStatus(status, options);

		// build promise chain
    	let middlewarePreObservable = of(requestContext);
    	for (let middleware of this.configuration.middleware) {
    		middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
    	}

    	return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => this.configuration.httpApi.send(ctx))).
	    	pipe(mergeMap((response: ResponseContext) => {
	    		let middlewarePostObservable = of(response);
	    		for (let middleware of this.configuration.middleware) {
	    			middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
	    		}
	    		return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.findPetsByStatus(rsp)));
	    	}));
    }
	
    public findPetsByTags(tags: Array<string>, options?: Configuration): Observable<Array<Pet>> {
    	const requestContext = this.requestFactory.findPetsByTags(tags, options);

		// build promise chain
    	let middlewarePreObservable = of(requestContext);
    	for (let middleware of this.configuration.middleware) {
    		middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
    	}

    	return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => this.configuration.httpApi.send(ctx))).
	    	pipe(mergeMap((response: ResponseContext) => {
	    		let middlewarePostObservable = of(response);
	    		for (let middleware of this.configuration.middleware) {
	    			middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
	    		}
	    		return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.findPetsByTags(rsp)));
	    	}));
    }
	
    public getPetById(petId: number, options?: Configuration): Observable<Pet> {
    	const requestContext = this.requestFactory.getPetById(petId, options);

		// build promise chain
    	let middlewarePreObservable = of(requestContext);
    	for (let middleware of this.configuration.middleware) {
    		middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
    	}

    	return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => this.configuration.httpApi.send(ctx))).
	    	pipe(mergeMap((response: ResponseContext) => {
	    		let middlewarePostObservable = of(response);
	    		for (let middleware of this.configuration.middleware) {
	    			middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
	    		}
	    		return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.getPetById(rsp)));
	    	}));
    }
	
    public updatePet(pet: Pet, options?: Configuration): Observable<void> {
    	const requestContext = this.requestFactory.updatePet(pet, options);

		// build promise chain
    	let middlewarePreObservable = of(requestContext);
    	for (let middleware of this.configuration.middleware) {
    		middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
    	}

    	return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => this.configuration.httpApi.send(ctx))).
	    	pipe(mergeMap((response: ResponseContext) => {
	    		let middlewarePostObservable = of(response);
	    		for (let middleware of this.configuration.middleware) {
	    			middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
	    		}
	    		return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.updatePet(rsp)));
	    	}));
    }
	
    public updatePetWithForm(petId: number, name?: string, status?: string, options?: Configuration): Observable<void> {
    	const requestContext = this.requestFactory.updatePetWithForm(petId, name, status, options);

		// build promise chain
    	let middlewarePreObservable = of(requestContext);
    	for (let middleware of this.configuration.middleware) {
    		middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
    	}

    	return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => this.configuration.httpApi.send(ctx))).
	    	pipe(mergeMap((response: ResponseContext) => {
	    		let middlewarePostObservable = of(response);
	    		for (let middleware of this.configuration.middleware) {
	    			middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
	    		}
	    		return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.updatePetWithForm(rsp)));
	    	}));
    }
	
    public uploadFile(petId: number, additionalMetadata?: string, file?: HttpFile, options?: Configuration): Observable<ApiResponse> {
    	const requestContext = this.requestFactory.uploadFile(petId, additionalMetadata, file, options);

		// build promise chain
    	let middlewarePreObservable = of(requestContext);
    	for (let middleware of this.configuration.middleware) {
    		middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
    	}

    	return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => this.configuration.httpApi.send(ctx))).
	    	pipe(mergeMap((response: ResponseContext) => {
	    		let middlewarePostObservable = of(response);
	    		for (let middleware of this.configuration.middleware) {
	    			middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
	    		}
	    		return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.uploadFile(rsp)));
	    	}));
    }
	

}




import { StoreApiRequestFactory, StoreApiResponseProcessor} from "../apis/StoreApi";
export class ObservableStoreApi {
	private requestFactory: StoreApiRequestFactory;
	private responseProcessor: StoreApiResponseProcessor;
    private configuration: Configuration;
    
	public constructor(configuration: Configuration, requestFactory?: StoreApiRequestFactory, responseProcessor?: StoreApiResponseProcessor) {
	    this.configuration = configuration;
		this.requestFactory = requestFactory || new StoreApiRequestFactory(configuration);
		this.responseProcessor = responseProcessor || new StoreApiResponseProcessor();
	}

    public deleteOrder(orderId: string, options?: Configuration): Observable<void> {
    	const requestContext = this.requestFactory.deleteOrder(orderId, options);

		// build promise chain
    	let middlewarePreObservable = of(requestContext);
    	for (let middleware of this.configuration.middleware) {
    		middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
    	}

    	return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => this.configuration.httpApi.send(ctx))).
	    	pipe(mergeMap((response: ResponseContext) => {
	    		let middlewarePostObservable = of(response);
	    		for (let middleware of this.configuration.middleware) {
	    			middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
	    		}
	    		return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.deleteOrder(rsp)));
	    	}));
    }
	
    public getInventory(options?: Configuration): Observable<{ [key: string]: number; }> {
    	const requestContext = this.requestFactory.getInventory(options);

		// build promise chain
    	let middlewarePreObservable = of(requestContext);
    	for (let middleware of this.configuration.middleware) {
    		middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
    	}

    	return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => this.configuration.httpApi.send(ctx))).
	    	pipe(mergeMap((response: ResponseContext) => {
	    		let middlewarePostObservable = of(response);
	    		for (let middleware of this.configuration.middleware) {
	    			middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
	    		}
	    		return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.getInventory(rsp)));
	    	}));
    }
	
    public getOrderById(orderId: number, options?: Configuration): Observable<Order> {
    	const requestContext = this.requestFactory.getOrderById(orderId, options);

		// build promise chain
    	let middlewarePreObservable = of(requestContext);
    	for (let middleware of this.configuration.middleware) {
    		middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
    	}

    	return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => this.configuration.httpApi.send(ctx))).
	    	pipe(mergeMap((response: ResponseContext) => {
	    		let middlewarePostObservable = of(response);
	    		for (let middleware of this.configuration.middleware) {
	    			middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
	    		}
	    		return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.getOrderById(rsp)));
	    	}));
    }
	
    public placeOrder(order: Order, options?: Configuration): Observable<Order> {
    	const requestContext = this.requestFactory.placeOrder(order, options);

		// build promise chain
    	let middlewarePreObservable = of(requestContext);
    	for (let middleware of this.configuration.middleware) {
    		middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
    	}

    	return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => this.configuration.httpApi.send(ctx))).
	    	pipe(mergeMap((response: ResponseContext) => {
	    		let middlewarePostObservable = of(response);
	    		for (let middleware of this.configuration.middleware) {
	    			middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
	    		}
	    		return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.placeOrder(rsp)));
	    	}));
    }
	

}




import { UserApiRequestFactory, UserApiResponseProcessor} from "../apis/UserApi";
export class ObservableUserApi {
	private requestFactory: UserApiRequestFactory;
	private responseProcessor: UserApiResponseProcessor;
    private configuration: Configuration;
    
	public constructor(configuration: Configuration, requestFactory?: UserApiRequestFactory, responseProcessor?: UserApiResponseProcessor) {
	    this.configuration = configuration;
		this.requestFactory = requestFactory || new UserApiRequestFactory(configuration);
		this.responseProcessor = responseProcessor || new UserApiResponseProcessor();
	}

    public createUser(user: User, options?: Configuration): Observable<void> {
    	const requestContext = this.requestFactory.createUser(user, options);

		// build promise chain
    	let middlewarePreObservable = of(requestContext);
    	for (let middleware of this.configuration.middleware) {
    		middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
    	}

    	return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => this.configuration.httpApi.send(ctx))).
	    	pipe(mergeMap((response: ResponseContext) => {
	    		let middlewarePostObservable = of(response);
	    		for (let middleware of this.configuration.middleware) {
	    			middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
	    		}
	    		return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.createUser(rsp)));
	    	}));
    }
	
    public createUsersWithArrayInput(user: Array<User>, options?: Configuration): Observable<void> {
    	const requestContext = this.requestFactory.createUsersWithArrayInput(user, options);

		// build promise chain
    	let middlewarePreObservable = of(requestContext);
    	for (let middleware of this.configuration.middleware) {
    		middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
    	}

    	return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => this.configuration.httpApi.send(ctx))).
	    	pipe(mergeMap((response: ResponseContext) => {
	    		let middlewarePostObservable = of(response);
	    		for (let middleware of this.configuration.middleware) {
	    			middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
	    		}
	    		return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.createUsersWithArrayInput(rsp)));
	    	}));
    }
	
    public createUsersWithListInput(user: Array<User>, options?: Configuration): Observable<void> {
    	const requestContext = this.requestFactory.createUsersWithListInput(user, options);

		// build promise chain
    	let middlewarePreObservable = of(requestContext);
    	for (let middleware of this.configuration.middleware) {
    		middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
    	}

    	return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => this.configuration.httpApi.send(ctx))).
	    	pipe(mergeMap((response: ResponseContext) => {
	    		let middlewarePostObservable = of(response);
	    		for (let middleware of this.configuration.middleware) {
	    			middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
	    		}
	    		return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.createUsersWithListInput(rsp)));
	    	}));
    }
	
    public deleteUser(username: string, options?: Configuration): Observable<void> {
    	const requestContext = this.requestFactory.deleteUser(username, options);

		// build promise chain
    	let middlewarePreObservable = of(requestContext);
    	for (let middleware of this.configuration.middleware) {
    		middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
    	}

    	return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => this.configuration.httpApi.send(ctx))).
	    	pipe(mergeMap((response: ResponseContext) => {
	    		let middlewarePostObservable = of(response);
	    		for (let middleware of this.configuration.middleware) {
	    			middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
	    		}
	    		return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.deleteUser(rsp)));
	    	}));
    }
	
    public getUserByName(username: string, options?: Configuration): Observable<User> {
    	const requestContext = this.requestFactory.getUserByName(username, options);

		// build promise chain
    	let middlewarePreObservable = of(requestContext);
    	for (let middleware of this.configuration.middleware) {
    		middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
    	}

    	return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => this.configuration.httpApi.send(ctx))).
	    	pipe(mergeMap((response: ResponseContext) => {
	    		let middlewarePostObservable = of(response);
	    		for (let middleware of this.configuration.middleware) {
	    			middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
	    		}
	    		return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.getUserByName(rsp)));
	    	}));
    }
	
    public loginUser(username: string, password: string, options?: Configuration): Observable<string> {
    	const requestContext = this.requestFactory.loginUser(username, password, options);

		// build promise chain
    	let middlewarePreObservable = of(requestContext);
    	for (let middleware of this.configuration.middleware) {
    		middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
    	}

    	return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => this.configuration.httpApi.send(ctx))).
	    	pipe(mergeMap((response: ResponseContext) => {
	    		let middlewarePostObservable = of(response);
	    		for (let middleware of this.configuration.middleware) {
	    			middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
	    		}
	    		return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.loginUser(rsp)));
	    	}));
    }
	
    public logoutUser(options?: Configuration): Observable<void> {
    	const requestContext = this.requestFactory.logoutUser(options);

		// build promise chain
    	let middlewarePreObservable = of(requestContext);
    	for (let middleware of this.configuration.middleware) {
    		middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
    	}

    	return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => this.configuration.httpApi.send(ctx))).
	    	pipe(mergeMap((response: ResponseContext) => {
	    		let middlewarePostObservable = of(response);
	    		for (let middleware of this.configuration.middleware) {
	    			middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
	    		}
	    		return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.logoutUser(rsp)));
	    	}));
    }
	
    public updateUser(username: string, user: User, options?: Configuration): Observable<void> {
    	const requestContext = this.requestFactory.updateUser(username, user, options);

		// build promise chain
    	let middlewarePreObservable = of(requestContext);
    	for (let middleware of this.configuration.middleware) {
    		middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
    	}

    	return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => this.configuration.httpApi.send(ctx))).
	    	pipe(mergeMap((response: ResponseContext) => {
	    		let middlewarePostObservable = of(response);
	    		for (let middleware of this.configuration.middleware) {
	    			middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
	    		}
	    		return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.updateUser(rsp)));
	    	}));
    }
	

}



