import type { HttpFile } from "../http/http";
import type { Observable } from "../rxjsStub";
import type { Configuration } from "../configuration";
import type { Middleware } from "../middleware";

import { ApiResponse } from "../models/ApiResponse";
import { Category } from "../models/Category";
import { Order } from "../models/Order";
import { Pet } from "../models/Pet";
import { Tag } from "../models/Tag";
import { User } from "../models/User";


export abstract class AbstractObservablePetApi {
    public abstract addPet(pet: Pet, options?: Configuration | Middleware[]): Observable<Pet>;

    public abstract deletePet(petId: number, apiKey?: string, options?: Configuration | Middleware[]): Observable<void>;

    public abstract findPetsByStatus(status: Array<'available' | 'pending' | 'sold'>, options?: Configuration | Middleware[]): Observable<Array<Pet>>;

    public abstract findPetsByTags(tags: Array<string>, options?: Configuration | Middleware[]): Observable<Array<Pet>>;

    public abstract getPetById(petId: number, options?: Configuration | Middleware[]): Observable<Pet>;

    public abstract updatePet(pet: Pet, options?: Configuration | Middleware[]): Observable<Pet>;

    public abstract updatePetWithForm(petId: number, name?: string, status?: string, options?: Configuration | Middleware[]): Observable<void>;

    public abstract uploadFile(petId: number, additionalMetadata?: string, file?: HttpFile, options?: Configuration | Middleware[]): Observable<ApiResponse>;

}


export abstract class AbstractObservableStoreApi {
    public abstract deleteOrder(orderId: string, options?: Configuration | Middleware[]): Observable<void>;

    public abstract getInventory(options?: Configuration | Middleware[]): Observable<{ [key: string]: number; }>;

    public abstract getOrderById(orderId: number, options?: Configuration | Middleware[]): Observable<Order>;

    public abstract placeOrder(order: Order, options?: Configuration | Middleware[]): Observable<Order>;

}


export abstract class AbstractObservableUserApi {
    public abstract createUser(user: User, options?: Configuration | Middleware[]): Observable<void>;

    public abstract createUsersWithArrayInput(user: Array<User>, options?: Configuration | Middleware[]): Observable<void>;

    public abstract createUsersWithListInput(user: Array<User>, options?: Configuration | Middleware[]): Observable<void>;

    public abstract deleteUser(username: string, options?: Configuration | Middleware[]): Observable<void>;

    public abstract getUserByName(username: string, options?: Configuration | Middleware[]): Observable<User>;

    public abstract loginUser(username: string, password: string, options?: Configuration | Middleware[]): Observable<string>;

    public abstract logoutUser(options?: Configuration | Middleware[]): Observable<void>;

    public abstract updateUser(username: string, user: User, options?: Configuration | Middleware[]): Observable<void>;

}
