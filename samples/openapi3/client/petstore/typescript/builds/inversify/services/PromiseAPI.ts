import type { HttpFile } from "../http/http";
import type { Configuration } from "../configuration";

import { ApiResponse } from "../models/ApiResponse";
import { Category } from "../models/Category";
import { Order } from "../models/Order";
import { Pet } from "../models/Pet";
import { Tag } from "../models/Tag";
import { User } from "../models/User";


export abstract class AbstractPromisePetApi {
    public abstract addPet(pet: Pet, options?: Configuration): Promise<Pet>;

    public abstract deletePet(petId: number, apiKey?: string, options?: Configuration): Promise<void>;

    public abstract findPetsByStatus(status: Array<'available' | 'pending' | 'sold'>, options?: Configuration): Promise<Array<Pet>>;

    public abstract findPetsByTags(tags: Array<string>, options?: Configuration): Promise<Array<Pet>>;

    public abstract getPetById(petId: number, options?: Configuration): Promise<Pet>;

    public abstract updatePet(pet: Pet, options?: Configuration): Promise<Pet>;

    public abstract updatePetWithForm(petId: number, name?: string, status?: string, options?: Configuration): Promise<void>;

    public abstract uploadFile(petId: number, additionalMetadata?: string, file?: HttpFile, options?: Configuration): Promise<ApiResponse>;

}


export abstract class AbstractPromiseStoreApi {
    public abstract deleteOrder(orderId: string, options?: Configuration): Promise<void>;

    public abstract getInventory(options?: Configuration): Promise<{ [key: string]: number; }>;

    public abstract getOrderById(orderId: number, options?: Configuration): Promise<Order>;

    public abstract placeOrder(order: Order, options?: Configuration): Promise<Order>;

}


export abstract class AbstractPromiseUserApi {
    public abstract createUser(user: User, options?: Configuration): Promise<void>;

    public abstract createUsersWithArrayInput(user: Array<User>, options?: Configuration): Promise<void>;

    public abstract createUsersWithListInput(user: Array<User>, options?: Configuration): Promise<void>;

    public abstract deleteUser(username: string, options?: Configuration): Promise<void>;

    public abstract getUserByName(username: string, options?: Configuration): Promise<User>;

    public abstract loginUser(username: string, password: string, options?: Configuration): Promise<string>;

    public abstract logoutUser(options?: Configuration): Promise<void>;

    public abstract updateUser(username: string, user: User, options?: Configuration): Promise<void>;

}
