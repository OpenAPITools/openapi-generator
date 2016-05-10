import * as querystring from "querystring";
import * as url from "url";

import * as isomorphicFetch from "isomorphic-fetch";
import * as assign from "core-js/library/fn/object/assign";

interface Dictionary<T> { [index: string]: T; }
export interface FetchAPI { (url: string, init?: any): Promise<any>; }

export class BaseAPI {
    basePath: string;
    fetch: FetchAPI;

    constructor(basePath: string = "http://petstore.swagger.io/v2", fetch: FetchAPI = isomorphicFetch) {
        this.basePath = basePath;
        this.fetch = fetch;
    }
}

export interface Category {
    "id"?: number;
    "name"?: string;
}

export interface Order {
    "id"?: number;
    "petId"?: number;
    "quantity"?: number;
    "shipDate"?: Date;
    /**
     * Order Status
     */
    "status"?: OrderStatusEnum;
    "complete"?: boolean;
}

export type OrderStatusEnum = "placed" | "approved" | "delivered";
export interface Pet {
    "id"?: number;
    "category"?: Category;
    "name": string;
    "photoUrls": Array<string>;
    "tags"?: Array<Tag>;
    /**
     * pet status in the store
     */
    "status"?: PetStatusEnum;
}

export type PetStatusEnum = "available" | "pending" | "sold";
export interface Tag {
    "id"?: number;
    "name"?: string;
}

export interface User {
    "id"?: number;
    "username"?: string;
    "firstName"?: string;
    "lastName"?: string;
    "email"?: string;
    "password"?: string;
    "phone"?: string;
    /**
     * User Status
     */
    "userStatus"?: number;
}



export class PetApi extends BaseAPI {
    /** 
     * Add a new pet to the store
     * 
     * @param body Pet object that needs to be added to the store
     */
    addPet(params: {  body?: Pet; }): Promise<any> {
        const baseUrl = `${this.basePath}/pet`;
        let urlObj = url.parse(baseUrl, true);
        let fetchOptions: RequestInit = { method: "POST" };

        let contentTypeHeader: Dictionary<string>;
        contentTypeHeader = { "Content-Type": "application/json" };
        if (params["body"]) {
            fetchOptions.body = JSON.stringify(params["body"] || {});
        }
        if (contentTypeHeader) {
            fetchOptions.headers = contentTypeHeader;
        }
        return this.fetch(url.format(urlObj), fetchOptions).then((response) => {
            if (response.status >= 200 && response.status < 300) {
                return response;
            } else {
                throw response;
            }
        });
    }
    /** 
     * Deletes a pet
     * 
     * @param petId Pet id to delete
     * @param apiKey 
     */
    deletePet(params: {  petId: number; apiKey?: string; }): Promise<any> {
        // verify required parameter "petId" is set
        if (params["petId"] == null) {
            throw new Error("Missing required parameter petId when calling deletePet");
        }
        const baseUrl = `${this.basePath}/pet/{petId}`
            .replace(`{${"petId"}}`, `${ params.petId }`);
        let urlObj = url.parse(baseUrl, true);
        let fetchOptions: RequestInit = { method: "DELETE" };

        let contentTypeHeader: Dictionary<string>;
        if (contentTypeHeader) {
            fetchOptions.headers = contentTypeHeader;
        }
        return this.fetch(url.format(urlObj), fetchOptions).then((response) => {
            if (response.status >= 200 && response.status < 300) {
                return response;
            } else {
                throw response;
            }
        });
    }
    /** 
     * Finds Pets by status
     * Multiple status values can be provided with comma seperated strings
     * @param status Status values that need to be considered for filter
     */
    findPetsByStatus(params: {  status?: Array<string>; }): Promise<Array<Pet>> {
        const baseUrl = `${this.basePath}/pet/findByStatus`;
        let urlObj = url.parse(baseUrl, true);
        urlObj.query = assign({}, urlObj.query, { 
            "status": params.status,
        });
        let fetchOptions: RequestInit = { method: "GET" };

        let contentTypeHeader: Dictionary<string>;
        if (contentTypeHeader) {
            fetchOptions.headers = contentTypeHeader;
        }
        return this.fetch(url.format(urlObj), fetchOptions).then((response) => {
            if (response.status >= 200 && response.status < 300) {
                return response.json();
            } else {
                throw response;
            }
        });
    }
    /** 
     * Finds Pets by tags
     * Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
     * @param tags Tags to filter by
     */
    findPetsByTags(params: {  tags?: Array<string>; }): Promise<Array<Pet>> {
        const baseUrl = `${this.basePath}/pet/findByTags`;
        let urlObj = url.parse(baseUrl, true);
        urlObj.query = assign({}, urlObj.query, { 
            "tags": params.tags,
        });
        let fetchOptions: RequestInit = { method: "GET" };

        let contentTypeHeader: Dictionary<string>;
        if (contentTypeHeader) {
            fetchOptions.headers = contentTypeHeader;
        }
        return this.fetch(url.format(urlObj), fetchOptions).then((response) => {
            if (response.status >= 200 && response.status < 300) {
                return response.json();
            } else {
                throw response;
            }
        });
    }
    /** 
     * Find pet by ID
     * Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
     * @param petId ID of pet that needs to be fetched
     */
    getPetById(params: {  petId: number; }): Promise<Pet> {
        // verify required parameter "petId" is set
        if (params["petId"] == null) {
            throw new Error("Missing required parameter petId when calling getPetById");
        }
        const baseUrl = `${this.basePath}/pet/{petId}`
            .replace(`{${"petId"}}`, `${ params.petId }`);
        let urlObj = url.parse(baseUrl, true);
        let fetchOptions: RequestInit = { method: "GET" };

        let contentTypeHeader: Dictionary<string>;
        if (contentTypeHeader) {
            fetchOptions.headers = contentTypeHeader;
        }
        return this.fetch(url.format(urlObj), fetchOptions).then((response) => {
            if (response.status >= 200 && response.status < 300) {
                return response.json();
            } else {
                throw response;
            }
        });
    }
    /** 
     * Update an existing pet
     * 
     * @param body Pet object that needs to be added to the store
     */
    updatePet(params: {  body?: Pet; }): Promise<any> {
        const baseUrl = `${this.basePath}/pet`;
        let urlObj = url.parse(baseUrl, true);
        let fetchOptions: RequestInit = { method: "PUT" };

        let contentTypeHeader: Dictionary<string>;
        contentTypeHeader = { "Content-Type": "application/json" };
        if (params["body"]) {
            fetchOptions.body = JSON.stringify(params["body"] || {});
        }
        if (contentTypeHeader) {
            fetchOptions.headers = contentTypeHeader;
        }
        return this.fetch(url.format(urlObj), fetchOptions).then((response) => {
            if (response.status >= 200 && response.status < 300) {
                return response;
            } else {
                throw response;
            }
        });
    }
    /** 
     * Updates a pet in the store with form data
     * 
     * @param petId ID of pet that needs to be updated
     * @param name Updated name of the pet
     * @param status Updated status of the pet
     */
    updatePetWithForm(params: {  petId: string; name?: string; status?: string; }): Promise<any> {
        // verify required parameter "petId" is set
        if (params["petId"] == null) {
            throw new Error("Missing required parameter petId when calling updatePetWithForm");
        }
        const baseUrl = `${this.basePath}/pet/{petId}`
            .replace(`{${"petId"}}`, `${ params.petId }`);
        let urlObj = url.parse(baseUrl, true);
        let fetchOptions: RequestInit = { method: "POST" };

        let contentTypeHeader: Dictionary<string>;
        contentTypeHeader = { "Content-Type": "application/x-www-form-urlencoded" };
        fetchOptions.body = querystring.stringify({ 
            "name": params.name,
            "status": params.status,
        });
        if (contentTypeHeader) {
            fetchOptions.headers = contentTypeHeader;
        }
        return this.fetch(url.format(urlObj), fetchOptions).then((response) => {
            if (response.status >= 200 && response.status < 300) {
                return response;
            } else {
                throw response;
            }
        });
    }
    /** 
     * uploads an image
     * 
     * @param petId ID of pet to update
     * @param additionalMetadata Additional data to pass to server
     * @param file file to upload
     */
    uploadFile(params: {  petId: number; additionalMetadata?: string; file?: any; }): Promise<any> {
        // verify required parameter "petId" is set
        if (params["petId"] == null) {
            throw new Error("Missing required parameter petId when calling uploadFile");
        }
        const baseUrl = `${this.basePath}/pet/{petId}/uploadImage`
            .replace(`{${"petId"}}`, `${ params.petId }`);
        let urlObj = url.parse(baseUrl, true);
        let fetchOptions: RequestInit = { method: "POST" };

        let contentTypeHeader: Dictionary<string>;
        contentTypeHeader = { "Content-Type": "application/x-www-form-urlencoded" };
        fetchOptions.body = querystring.stringify({ 
            "additionalMetadata": params.additionalMetadata,
            "file": params.file,
        });
        if (contentTypeHeader) {
            fetchOptions.headers = contentTypeHeader;
        }
        return this.fetch(url.format(urlObj), fetchOptions).then((response) => {
            if (response.status >= 200 && response.status < 300) {
                return response;
            } else {
                throw response;
            }
        });
    }
}


export class StoreApi extends BaseAPI {
    /** 
     * Delete purchase order by ID
     * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
     * @param orderId ID of the order that needs to be deleted
     */
    deleteOrder(params: {  orderId: string; }): Promise<any> {
        // verify required parameter "orderId" is set
        if (params["orderId"] == null) {
            throw new Error("Missing required parameter orderId when calling deleteOrder");
        }
        const baseUrl = `${this.basePath}/store/order/{orderId}`
            .replace(`{${"orderId"}}`, `${ params.orderId }`);
        let urlObj = url.parse(baseUrl, true);
        let fetchOptions: RequestInit = { method: "DELETE" };

        let contentTypeHeader: Dictionary<string>;
        if (contentTypeHeader) {
            fetchOptions.headers = contentTypeHeader;
        }
        return this.fetch(url.format(urlObj), fetchOptions).then((response) => {
            if (response.status >= 200 && response.status < 300) {
                return response;
            } else {
                throw response;
            }
        });
    }
    /** 
     * Returns pet inventories by status
     * Returns a map of status codes to quantities
     */
    getInventory(): Promise<{ [key: string]: number; }> {
        const baseUrl = `${this.basePath}/store/inventory`;
        let urlObj = url.parse(baseUrl, true);
        let fetchOptions: RequestInit = { method: "GET" };

        let contentTypeHeader: Dictionary<string>;
        if (contentTypeHeader) {
            fetchOptions.headers = contentTypeHeader;
        }
        return this.fetch(url.format(urlObj), fetchOptions).then((response) => {
            if (response.status >= 200 && response.status < 300) {
                return response.json();
            } else {
                throw response;
            }
        });
    }
    /** 
     * Find purchase order by ID
     * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
     * @param orderId ID of pet that needs to be fetched
     */
    getOrderById(params: {  orderId: string; }): Promise<Order> {
        // verify required parameter "orderId" is set
        if (params["orderId"] == null) {
            throw new Error("Missing required parameter orderId when calling getOrderById");
        }
        const baseUrl = `${this.basePath}/store/order/{orderId}`
            .replace(`{${"orderId"}}`, `${ params.orderId }`);
        let urlObj = url.parse(baseUrl, true);
        let fetchOptions: RequestInit = { method: "GET" };

        let contentTypeHeader: Dictionary<string>;
        if (contentTypeHeader) {
            fetchOptions.headers = contentTypeHeader;
        }
        return this.fetch(url.format(urlObj), fetchOptions).then((response) => {
            if (response.status >= 200 && response.status < 300) {
                return response.json();
            } else {
                throw response;
            }
        });
    }
    /** 
     * Place an order for a pet
     * 
     * @param body order placed for purchasing the pet
     */
    placeOrder(params: {  body?: Order; }): Promise<Order> {
        const baseUrl = `${this.basePath}/store/order`;
        let urlObj = url.parse(baseUrl, true);
        let fetchOptions: RequestInit = { method: "POST" };

        let contentTypeHeader: Dictionary<string>;
        contentTypeHeader = { "Content-Type": "application/json" };
        if (params["body"]) {
            fetchOptions.body = JSON.stringify(params["body"] || {});
        }
        if (contentTypeHeader) {
            fetchOptions.headers = contentTypeHeader;
        }
        return this.fetch(url.format(urlObj), fetchOptions).then((response) => {
            if (response.status >= 200 && response.status < 300) {
                return response.json();
            } else {
                throw response;
            }
        });
    }
}


export class UserApi extends BaseAPI {
    /** 
     * Create user
     * This can only be done by the logged in user.
     * @param body Created user object
     */
    createUser(params: {  body?: User; }): Promise<any> {
        const baseUrl = `${this.basePath}/user`;
        let urlObj = url.parse(baseUrl, true);
        let fetchOptions: RequestInit = { method: "POST" };

        let contentTypeHeader: Dictionary<string>;
        contentTypeHeader = { "Content-Type": "application/json" };
        if (params["body"]) {
            fetchOptions.body = JSON.stringify(params["body"] || {});
        }
        if (contentTypeHeader) {
            fetchOptions.headers = contentTypeHeader;
        }
        return this.fetch(url.format(urlObj), fetchOptions).then((response) => {
            if (response.status >= 200 && response.status < 300) {
                return response;
            } else {
                throw response;
            }
        });
    }
    /** 
     * Creates list of users with given input array
     * 
     * @param body List of user object
     */
    createUsersWithArrayInput(params: {  body?: Array<User>; }): Promise<any> {
        const baseUrl = `${this.basePath}/user/createWithArray`;
        let urlObj = url.parse(baseUrl, true);
        let fetchOptions: RequestInit = { method: "POST" };

        let contentTypeHeader: Dictionary<string>;
        contentTypeHeader = { "Content-Type": "application/json" };
        if (params["body"]) {
            fetchOptions.body = JSON.stringify(params["body"] || {});
        }
        if (contentTypeHeader) {
            fetchOptions.headers = contentTypeHeader;
        }
        return this.fetch(url.format(urlObj), fetchOptions).then((response) => {
            if (response.status >= 200 && response.status < 300) {
                return response;
            } else {
                throw response;
            }
        });
    }
    /** 
     * Creates list of users with given input array
     * 
     * @param body List of user object
     */
    createUsersWithListInput(params: {  body?: Array<User>; }): Promise<any> {
        const baseUrl = `${this.basePath}/user/createWithList`;
        let urlObj = url.parse(baseUrl, true);
        let fetchOptions: RequestInit = { method: "POST" };

        let contentTypeHeader: Dictionary<string>;
        contentTypeHeader = { "Content-Type": "application/json" };
        if (params["body"]) {
            fetchOptions.body = JSON.stringify(params["body"] || {});
        }
        if (contentTypeHeader) {
            fetchOptions.headers = contentTypeHeader;
        }
        return this.fetch(url.format(urlObj), fetchOptions).then((response) => {
            if (response.status >= 200 && response.status < 300) {
                return response;
            } else {
                throw response;
            }
        });
    }
    /** 
     * Delete user
     * This can only be done by the logged in user.
     * @param username The name that needs to be deleted
     */
    deleteUser(params: {  username: string; }): Promise<any> {
        // verify required parameter "username" is set
        if (params["username"] == null) {
            throw new Error("Missing required parameter username when calling deleteUser");
        }
        const baseUrl = `${this.basePath}/user/{username}`
            .replace(`{${"username"}}`, `${ params.username }`);
        let urlObj = url.parse(baseUrl, true);
        let fetchOptions: RequestInit = { method: "DELETE" };

        let contentTypeHeader: Dictionary<string>;
        if (contentTypeHeader) {
            fetchOptions.headers = contentTypeHeader;
        }
        return this.fetch(url.format(urlObj), fetchOptions).then((response) => {
            if (response.status >= 200 && response.status < 300) {
                return response;
            } else {
                throw response;
            }
        });
    }
    /** 
     * Get user by user name
     * 
     * @param username The name that needs to be fetched. Use user1 for testing. 
     */
    getUserByName(params: {  username: string; }): Promise<User> {
        // verify required parameter "username" is set
        if (params["username"] == null) {
            throw new Error("Missing required parameter username when calling getUserByName");
        }
        const baseUrl = `${this.basePath}/user/{username}`
            .replace(`{${"username"}}`, `${ params.username }`);
        let urlObj = url.parse(baseUrl, true);
        let fetchOptions: RequestInit = { method: "GET" };

        let contentTypeHeader: Dictionary<string>;
        if (contentTypeHeader) {
            fetchOptions.headers = contentTypeHeader;
        }
        return this.fetch(url.format(urlObj), fetchOptions).then((response) => {
            if (response.status >= 200 && response.status < 300) {
                return response.json();
            } else {
                throw response;
            }
        });
    }
    /** 
     * Logs user into the system
     * 
     * @param username The user name for login
     * @param password The password for login in clear text
     */
    loginUser(params: {  username?: string; password?: string; }): Promise<string> {
        const baseUrl = `${this.basePath}/user/login`;
        let urlObj = url.parse(baseUrl, true);
        urlObj.query = assign({}, urlObj.query, { 
            "username": params.username,
            "password": params.password,
        });
        let fetchOptions: RequestInit = { method: "GET" };

        let contentTypeHeader: Dictionary<string>;
        if (contentTypeHeader) {
            fetchOptions.headers = contentTypeHeader;
        }
        return this.fetch(url.format(urlObj), fetchOptions).then((response) => {
            if (response.status >= 200 && response.status < 300) {
                return response.json();
            } else {
                throw response;
            }
        });
    }
    /** 
     * Logs out current logged in user session
     * 
     */
    logoutUser(): Promise<any> {
        const baseUrl = `${this.basePath}/user/logout`;
        let urlObj = url.parse(baseUrl, true);
        let fetchOptions: RequestInit = { method: "GET" };

        let contentTypeHeader: Dictionary<string>;
        if (contentTypeHeader) {
            fetchOptions.headers = contentTypeHeader;
        }
        return this.fetch(url.format(urlObj), fetchOptions).then((response) => {
            if (response.status >= 200 && response.status < 300) {
                return response;
            } else {
                throw response;
            }
        });
    }
    /** 
     * Updated user
     * This can only be done by the logged in user.
     * @param username name that need to be deleted
     * @param body Updated user object
     */
    updateUser(params: {  username: string; body?: User; }): Promise<any> {
        // verify required parameter "username" is set
        if (params["username"] == null) {
            throw new Error("Missing required parameter username when calling updateUser");
        }
        const baseUrl = `${this.basePath}/user/{username}`
            .replace(`{${"username"}}`, `${ params.username }`);
        let urlObj = url.parse(baseUrl, true);
        let fetchOptions: RequestInit = { method: "PUT" };

        let contentTypeHeader: Dictionary<string>;
        contentTypeHeader = { "Content-Type": "application/json" };
        if (params["body"]) {
            fetchOptions.body = JSON.stringify(params["body"] || {});
        }
        if (contentTypeHeader) {
            fetchOptions.headers = contentTypeHeader;
        }
        return this.fetch(url.format(urlObj), fetchOptions).then((response) => {
            if (response.status >= 200 && response.status < 300) {
                return response;
            } else {
                throw response;
            }
        });
    }
}

