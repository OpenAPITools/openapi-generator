import type { HttpFile } from "../http/http";
import type { Observable } from "../rxjsStub";
import type { Configuration } from "../configuration";

import { ApiResponse } from "../models/ApiResponse";
import { Category } from "../models/Category";
import { Order } from "../models/Order";
import { Pet } from "../models/Pet";
import { Tag } from "../models/Tag";
import { User } from "../models/User";


export abstract class AbstractObservablePetApi {
    public abstract addPet(pet: Pet, options?: Configuration): Observable<Pet>;

    public abstract deletePet(petId: number, apiKey?: string, options?: Configuration): Observable<void>;

    public abstract findPetsByStatus(status: Array<'available' | 'pending' | 'sold'>, options?: Configuration): Observable<Array<Pet>>;

    public abstract findPetsByTags(tags: Array<string>, options?: Configuration): Observable<Array<Pet>>;

    public abstract getPetById(petId: number, options?: Configuration): Observable<Pet>;

    public abstract updatePet(pet: Pet, options?: Configuration): Observable<Pet>;

    public abstract updatePetWithForm(petId: number, name?: string, status?: string, options?: Configuration): Observable<void>;

    public abstract uploadFile(petId: number, additionalMetadata?: string, file?: HttpFile, options?: Configuration): Observable<ApiResponse>;

}


export abstract class AbstractObservableStoreApi {
    public abstract deleteOrder(orderId: string, options?: Configuration): Observable<void>;

    public abstract getInventory(options?: Configuration): Observable<{ [key: string]: number; }>;

    public abstract getOrderById(orderId: number, options?: Configuration): Observable<Order>;

    public abstract placeOrder(order: Order, options?: Configuration): Observable<Order>;

}


export abstract class AbstractObservableUserApi {
    public abstract createUser(user: User, options?: Configuration): Observable<void>;

    public abstract createUsersWithArrayInput(user: Array<User>, options?: Configuration): Observable<void>;

    public abstract createUsersWithListInput(user: Array<User>, options?: Configuration): Observable<void>;

    public abstract deleteUser(username: string, options?: Configuration): Observable<void>;

    public abstract getUserByName(username: string, options?: Configuration): Observable<User>;

    public abstract loginUser(username: string, password: string, options?: Configuration): Observable<string>;

    public abstract logoutUser(options?: Configuration): Observable<void>;

    public abstract updateUser(username: string, user: User, options?: Configuration): Observable<void>;

}
