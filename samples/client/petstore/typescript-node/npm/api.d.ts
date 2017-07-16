import request = require('request');
import http = require('http');
import Promise = require('bluebird');
export declare class Category {
    'id': number;
    'name': string;
    static discriminator: any;
    static attributeTypeMap: Array<{
        name: string;
        baseName: string;
        type: string;
    }>;
    static getAttributeTypeMap(): {
        name: string;
        baseName: string;
        type: string;
    }[];
}
export declare class Order {
    'id': number;
    'petId': number;
    'quantity': number;
    'shipDate': Date;
    'status': Order.StatusEnum;
    'complete': boolean;
    static discriminator: any;
    static attributeTypeMap: Array<{
        name: string;
        baseName: string;
        type: string;
    }>;
    static getAttributeTypeMap(): {
        name: string;
        baseName: string;
        type: string;
    }[];
}
export declare namespace Order {
    enum StatusEnum {
        Placed,
        Approved,
        Delivered,
    }
}
export declare class Pet {
    'id': number;
    'category': Category;
    'name': string;
    'photoUrls': Array<string>;
    'tags': Array<Tag>;
    'status': Pet.StatusEnum;
    static discriminator: any;
    static attributeTypeMap: Array<{
        name: string;
        baseName: string;
        type: string;
    }>;
    static getAttributeTypeMap(): {
        name: string;
        baseName: string;
        type: string;
    }[];
}
export declare namespace Pet {
    enum StatusEnum {
        Available,
        Pending,
        Sold,
    }
}
export declare class Tag {
    'id': number;
    'name': string;
    static discriminator: any;
    static attributeTypeMap: Array<{
        name: string;
        baseName: string;
        type: string;
    }>;
    static getAttributeTypeMap(): {
        name: string;
        baseName: string;
        type: string;
    }[];
}
export declare class User {
    'id': number;
    'username': string;
    'firstName': string;
    'lastName': string;
    'email': string;
    'password': string;
    'phone': string;
    'userStatus': number;
    static discriminator: any;
    static attributeTypeMap: Array<{
        name: string;
        baseName: string;
        type: string;
    }>;
    static getAttributeTypeMap(): {
        name: string;
        baseName: string;
        type: string;
    }[];
}
export interface Authentication {
    applyToRequest(requestOptions: request.Options): void;
}
export declare class HttpBasicAuth implements Authentication {
    username: string;
    password: string;
    applyToRequest(requestOptions: request.Options): void;
}
export declare class ApiKeyAuth implements Authentication {
    private location;
    private paramName;
    apiKey: string;
    constructor(location: string, paramName: string);
    applyToRequest(requestOptions: request.Options): void;
}
export declare class OAuth implements Authentication {
    accessToken: string;
    applyToRequest(requestOptions: request.Options): void;
}
export declare class VoidAuth implements Authentication {
    username: string;
    password: string;
    applyToRequest(_: request.Options): void;
}
export declare enum PetApiApiKeys {
    api_key = 0,
}
export declare class PetApi {
    protected basePath: string;
    protected defaultHeaders: any;
    protected _useQuerystring: boolean;
    protected authentications: {
        'default': Authentication;
        'api_key': ApiKeyAuth;
        'petstore_auth': OAuth;
    };
    constructor(basePath?: string);
    useQuerystring: boolean;
    setApiKey(key: PetApiApiKeys, value: string): void;
    accessToken: string;
    private extendObj<T1, T2>(objA, objB);
    addPet(body?: Pet): Promise<{
        response: http.ClientResponse;
        body?: any;
    }>;
    deletePet(petId: number, apiKey?: string): Promise<{
        response: http.ClientResponse;
        body?: any;
    }>;
    findPetsByStatus(status?: Array<string>): Promise<{
        response: http.ClientResponse;
        body: Array<Pet>;
    }>;
    findPetsByTags(tags?: Array<string>): Promise<{
        response: http.ClientResponse;
        body: Array<Pet>;
    }>;
    getPetById(petId: number): Promise<{
        response: http.ClientResponse;
        body: Pet;
    }>;
    updatePet(body?: Pet): Promise<{
        response: http.ClientResponse;
        body?: any;
    }>;
    updatePetWithForm(petId: string, name?: string, status?: string): Promise<{
        response: http.ClientResponse;
        body?: any;
    }>;
    uploadFile(petId: number, additionalMetadata?: string, file?: any): Promise<{
        response: http.ClientResponse;
        body?: any;
    }>;
}
export declare enum StoreApiApiKeys {
    api_key = 0,
}
export declare class StoreApi {
    protected basePath: string;
    protected defaultHeaders: any;
    protected _useQuerystring: boolean;
    protected authentications: {
        'default': Authentication;
        'api_key': ApiKeyAuth;
        'petstore_auth': OAuth;
    };
    constructor(basePath?: string);
    useQuerystring: boolean;
    setApiKey(key: StoreApiApiKeys, value: string): void;
    accessToken: string;
    private extendObj<T1, T2>(objA, objB);
    deleteOrder(orderId: string): Promise<{
        response: http.ClientResponse;
        body?: any;
    }>;
    getInventory(): Promise<{
        response: http.ClientResponse;
        body: {
            [key: string]: number;
        };
    }>;
    getOrderById(orderId: string): Promise<{
        response: http.ClientResponse;
        body: Order;
    }>;
    placeOrder(body?: Order): Promise<{
        response: http.ClientResponse;
        body: Order;
    }>;
}
export declare enum UserApiApiKeys {
    api_key = 0,
}
export declare class UserApi {
    protected basePath: string;
    protected defaultHeaders: any;
    protected _useQuerystring: boolean;
    protected authentications: {
        'default': Authentication;
        'api_key': ApiKeyAuth;
        'petstore_auth': OAuth;
    };
    constructor(basePath?: string);
    useQuerystring: boolean;
    setApiKey(key: UserApiApiKeys, value: string): void;
    accessToken: string;
    private extendObj<T1, T2>(objA, objB);
    createUser(body?: User): Promise<{
        response: http.ClientResponse;
        body?: any;
    }>;
    createUsersWithArrayInput(body?: Array<User>): Promise<{
        response: http.ClientResponse;
        body?: any;
    }>;
    createUsersWithListInput(body?: Array<User>): Promise<{
        response: http.ClientResponse;
        body?: any;
    }>;
    deleteUser(username: string): Promise<{
        response: http.ClientResponse;
        body?: any;
    }>;
    getUserByName(username: string): Promise<{
        response: http.ClientResponse;
        body: User;
    }>;
    loginUser(username?: string, password?: string): Promise<{
        response: http.ClientResponse;
        body: string;
    }>;
    logoutUser(): Promise<{
        response: http.ClientResponse;
        body?: any;
    }>;
    updateUser(username: string, body?: User): Promise<{
        response: http.ClientResponse;
        body?: any;
    }>;
}
