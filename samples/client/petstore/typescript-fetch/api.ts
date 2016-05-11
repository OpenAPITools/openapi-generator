import * as querystring from 'querystring';
import * as fetch from 'isomorphic-fetch';
import {assign} from './assign';


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
    "status"?: Order.StatusEnum;
    "complete"?: boolean;
}


export enum StatusEnum { 
    placed = <any> 'placed',
    approved = <any> 'approved',
    delivered = <any> 'delivered'
}
export interface Pet {
    "id"?: number;
    "category"?: Category;
    "name": string;
    "photoUrls": Array<string>;
    "tags"?: Array<Tag>;

    /**
     * pet status in the store
     */
    "status"?: Pet.StatusEnum;
}


export enum StatusEnum { 
    available = <any> 'available',
    pending = <any> 'pending',
    sold = <any> 'sold'
}
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


//export namespace  {
    'use strict';

    export class PetApi {
        protected basePath = 'http://petstore.swagger.io/v2';
        public defaultHeaders : any = {};

        constructor(basePath?: string) {
            if (basePath) {
                this.basePath = basePath;
            }
        }

        /**
         * Add a new pet to the store
         * 
         * @param body Pet object that needs to be added to the store
         */
        public addPet (params: {  body?: Pet; }, extraQueryParams?: any, extraFetchParams?: any ) : Promise<{}> {
            const localVarPath = this.basePath + '/pet';

            let queryParameters: any = assign({}, extraQueryParams);
            let headerParams: any = assign({}, this.defaultHeaders);
            headerParams['Content-Type'] = 'application/json';

            let fetchParams = {
                method: 'POST',
                headers: headerParams,
                body: JSON.stringify(params.body),
                
            };

            if (extraFetchParams) {
                fetchParams = assign(fetchParams, extraFetchParams);
            }

            let localVarPathWithQueryParameters = localVarPath + (localVarPath.indexOf('?') !== -1 ? '&' : '?') + querystring.stringify(queryParameters);

            return fetch(localVarPathWithQueryParameters, fetchParams).then((response) => {
                if (response.status >= 200 && response.status < 300) {
                    return response.json();
                } else {
                    var error = new Error(response.statusText);
                    error['response'] = response;
                    throw error;
                }
            });
        }
        /**
         * Deletes a pet
         * 
         * @param petId Pet id to delete
         * @param apiKey 
         */
        public deletePet (params: {  petId: number; apiKey?: string; }, extraQueryParams?: any, extraFetchParams?: any ) : Promise<{}> {
            const localVarPath = this.basePath + '/pet/{petId}'
                .replace('{' + 'petId' + '}', String(params.petId));

            let queryParameters: any = assign({}, extraQueryParams);
            let headerParams: any = assign({}, this.defaultHeaders);
            // verify required parameter 'petId' is set
            if (params.petId == null) {
                throw new Error('Missing required parameter petId when calling deletePet');
            }
            headerParams['api_key'] = params.apiKey;

            let fetchParams = {
                method: 'DELETE',
                headers: headerParams,
                                
            };

            if (extraFetchParams) {
                fetchParams = assign(fetchParams, extraFetchParams);
            }

            let localVarPathWithQueryParameters = localVarPath + (localVarPath.indexOf('?') !== -1 ? '&' : '?') + querystring.stringify(queryParameters);

            return fetch(localVarPathWithQueryParameters, fetchParams).then((response) => {
                if (response.status >= 200 && response.status < 300) {
                    return response.json();
                } else {
                    var error = new Error(response.statusText);
                    error['response'] = response;
                    throw error;
                }
            });
        }
        /**
         * Finds Pets by status
         * Multiple status values can be provided with comma seperated strings
         * @param status Status values that need to be considered for filter
         */
        public findPetsByStatus (params: {  status?: Array<string>; }, extraQueryParams?: any, extraFetchParams?: any ) : Promise<Array<Pet>> {
            const localVarPath = this.basePath + '/pet/findByStatus';

            let queryParameters: any = assign({}, extraQueryParams);
            let headerParams: any = assign({}, this.defaultHeaders);
            if (params.status !== undefined) {
                queryParameters['status'] = params.status;
            }

            let fetchParams = {
                method: 'GET',
                headers: headerParams,
                                
            };

            if (extraFetchParams) {
                fetchParams = assign(fetchParams, extraFetchParams);
            }

            let localVarPathWithQueryParameters = localVarPath + (localVarPath.indexOf('?') !== -1 ? '&' : '?') + querystring.stringify(queryParameters);

            return fetch(localVarPathWithQueryParameters, fetchParams).then((response) => {
                if (response.status >= 200 && response.status < 300) {
                    return response.json();
                } else {
                    var error = new Error(response.statusText);
                    error['response'] = response;
                    throw error;
                }
            });
        }
        /**
         * Finds Pets by tags
         * Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
         * @param tags Tags to filter by
         */
        public findPetsByTags (params: {  tags?: Array<string>; }, extraQueryParams?: any, extraFetchParams?: any ) : Promise<Array<Pet>> {
            const localVarPath = this.basePath + '/pet/findByTags';

            let queryParameters: any = assign({}, extraQueryParams);
            let headerParams: any = assign({}, this.defaultHeaders);
            if (params.tags !== undefined) {
                queryParameters['tags'] = params.tags;
            }

            let fetchParams = {
                method: 'GET',
                headers: headerParams,
                                
            };

            if (extraFetchParams) {
                fetchParams = assign(fetchParams, extraFetchParams);
            }

            let localVarPathWithQueryParameters = localVarPath + (localVarPath.indexOf('?') !== -1 ? '&' : '?') + querystring.stringify(queryParameters);

            return fetch(localVarPathWithQueryParameters, fetchParams).then((response) => {
                if (response.status >= 200 && response.status < 300) {
                    return response.json();
                } else {
                    var error = new Error(response.statusText);
                    error['response'] = response;
                    throw error;
                }
            });
        }
        /**
         * Find pet by ID
         * Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
         * @param petId ID of pet that needs to be fetched
         */
        public getPetById (params: {  petId: number; }, extraQueryParams?: any, extraFetchParams?: any ) : Promise<Pet> {
            const localVarPath = this.basePath + '/pet/{petId}'
                .replace('{' + 'petId' + '}', String(params.petId));

            let queryParameters: any = assign({}, extraQueryParams);
            let headerParams: any = assign({}, this.defaultHeaders);
            // verify required parameter 'petId' is set
            if (params.petId == null) {
                throw new Error('Missing required parameter petId when calling getPetById');
            }
            let fetchParams = {
                method: 'GET',
                headers: headerParams,
                                
            };

            if (extraFetchParams) {
                fetchParams = assign(fetchParams, extraFetchParams);
            }

            let localVarPathWithQueryParameters = localVarPath + (localVarPath.indexOf('?') !== -1 ? '&' : '?') + querystring.stringify(queryParameters);

            return fetch(localVarPathWithQueryParameters, fetchParams).then((response) => {
                if (response.status >= 200 && response.status < 300) {
                    return response.json();
                } else {
                    var error = new Error(response.statusText);
                    error['response'] = response;
                    throw error;
                }
            });
        }
        /**
         * Update an existing pet
         * 
         * @param body Pet object that needs to be added to the store
         */
        public updatePet (params: {  body?: Pet; }, extraQueryParams?: any, extraFetchParams?: any ) : Promise<{}> {
            const localVarPath = this.basePath + '/pet';

            let queryParameters: any = assign({}, extraQueryParams);
            let headerParams: any = assign({}, this.defaultHeaders);
            headerParams['Content-Type'] = 'application/json';

            let fetchParams = {
                method: 'PUT',
                headers: headerParams,
                body: JSON.stringify(params.body),
                
            };

            if (extraFetchParams) {
                fetchParams = assign(fetchParams, extraFetchParams);
            }

            let localVarPathWithQueryParameters = localVarPath + (localVarPath.indexOf('?') !== -1 ? '&' : '?') + querystring.stringify(queryParameters);

            return fetch(localVarPathWithQueryParameters, fetchParams).then((response) => {
                if (response.status >= 200 && response.status < 300) {
                    return response.json();
                } else {
                    var error = new Error(response.statusText);
                    error['response'] = response;
                    throw error;
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
        public updatePetWithForm (params: {  petId: string; name?: string; status?: string; }, extraQueryParams?: any, extraFetchParams?: any ) : Promise<{}> {
            const localVarPath = this.basePath + '/pet/{petId}'
                .replace('{' + 'petId' + '}', String(params.petId));

            let queryParameters: any = assign({}, extraQueryParams);
            let headerParams: any = assign({}, this.defaultHeaders);
            let formParams: any = {};
            headerParams['Content-Type'] = 'application/x-www-form-urlencoded';

            // verify required parameter 'petId' is set
            if (params.petId == null) {
                throw new Error('Missing required parameter petId when calling updatePetWithForm');
            }
            formParams['name'] = params.name;

            formParams['status'] = params.status;

            let fetchParams = {
                method: 'POST',
                headers: headerParams,
                                body: querystring.stringify(formParams),

            };

            if (extraFetchParams) {
                fetchParams = assign(fetchParams, extraFetchParams);
            }

            let localVarPathWithQueryParameters = localVarPath + (localVarPath.indexOf('?') !== -1 ? '&' : '?') + querystring.stringify(queryParameters);

            return fetch(localVarPathWithQueryParameters, fetchParams).then((response) => {
                if (response.status >= 200 && response.status < 300) {
                    return response.json();
                } else {
                    var error = new Error(response.statusText);
                    error['response'] = response;
                    throw error;
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
        public uploadFile (params: {  petId: number; additionalMetadata?: string; file?: any; }, extraQueryParams?: any, extraFetchParams?: any ) : Promise<{}> {
            const localVarPath = this.basePath + '/pet/{petId}/uploadImage'
                .replace('{' + 'petId' + '}', String(params.petId));

            let queryParameters: any = assign({}, extraQueryParams);
            let headerParams: any = assign({}, this.defaultHeaders);
            let formParams: any = {};
            headerParams['Content-Type'] = 'application/x-www-form-urlencoded';

            // verify required parameter 'petId' is set
            if (params.petId == null) {
                throw new Error('Missing required parameter petId when calling uploadFile');
            }
            formParams['additionalMetadata'] = params.additionalMetadata;

            formParams['file'] = params.file;

            let fetchParams = {
                method: 'POST',
                headers: headerParams,
                                body: querystring.stringify(formParams),

            };

            if (extraFetchParams) {
                fetchParams = assign(fetchParams, extraFetchParams);
            }

            let localVarPathWithQueryParameters = localVarPath + (localVarPath.indexOf('?') !== -1 ? '&' : '?') + querystring.stringify(queryParameters);

            return fetch(localVarPathWithQueryParameters, fetchParams).then((response) => {
                if (response.status >= 200 && response.status < 300) {
                    return response.json();
                } else {
                    var error = new Error(response.statusText);
                    error['response'] = response;
                    throw error;
                }
            });
        }
    }
//}
//export namespace  {
    'use strict';

    export class StoreApi {
        protected basePath = 'http://petstore.swagger.io/v2';
        public defaultHeaders : any = {};

        constructor(basePath?: string) {
            if (basePath) {
                this.basePath = basePath;
            }
        }

        /**
         * Delete purchase order by ID
         * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
         * @param orderId ID of the order that needs to be deleted
         */
        public deleteOrder (params: {  orderId: string; }, extraQueryParams?: any, extraFetchParams?: any ) : Promise<{}> {
            const localVarPath = this.basePath + '/store/order/{orderId}'
                .replace('{' + 'orderId' + '}', String(params.orderId));

            let queryParameters: any = assign({}, extraQueryParams);
            let headerParams: any = assign({}, this.defaultHeaders);
            // verify required parameter 'orderId' is set
            if (params.orderId == null) {
                throw new Error('Missing required parameter orderId when calling deleteOrder');
            }
            let fetchParams = {
                method: 'DELETE',
                headers: headerParams,
                                
            };

            if (extraFetchParams) {
                fetchParams = assign(fetchParams, extraFetchParams);
            }

            let localVarPathWithQueryParameters = localVarPath + (localVarPath.indexOf('?') !== -1 ? '&' : '?') + querystring.stringify(queryParameters);

            return fetch(localVarPathWithQueryParameters, fetchParams).then((response) => {
                if (response.status >= 200 && response.status < 300) {
                    return response.json();
                } else {
                    var error = new Error(response.statusText);
                    error['response'] = response;
                    throw error;
                }
            });
        }
        /**
         * Returns pet inventories by status
         * Returns a map of status codes to quantities
         */
        public getInventory (params: {  }, extraQueryParams?: any, extraFetchParams?: any ) : Promise<{ [key: string]: number; }> {
            const localVarPath = this.basePath + '/store/inventory';

            let queryParameters: any = assign({}, extraQueryParams);
            let headerParams: any = assign({}, this.defaultHeaders);
            let fetchParams = {
                method: 'GET',
                headers: headerParams,
                                
            };

            if (extraFetchParams) {
                fetchParams = assign(fetchParams, extraFetchParams);
            }

            let localVarPathWithQueryParameters = localVarPath + (localVarPath.indexOf('?') !== -1 ? '&' : '?') + querystring.stringify(queryParameters);

            return fetch(localVarPathWithQueryParameters, fetchParams).then((response) => {
                if (response.status >= 200 && response.status < 300) {
                    return response.json();
                } else {
                    var error = new Error(response.statusText);
                    error['response'] = response;
                    throw error;
                }
            });
        }
        /**
         * Find purchase order by ID
         * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
         * @param orderId ID of pet that needs to be fetched
         */
        public getOrderById (params: {  orderId: string; }, extraQueryParams?: any, extraFetchParams?: any ) : Promise<Order> {
            const localVarPath = this.basePath + '/store/order/{orderId}'
                .replace('{' + 'orderId' + '}', String(params.orderId));

            let queryParameters: any = assign({}, extraQueryParams);
            let headerParams: any = assign({}, this.defaultHeaders);
            // verify required parameter 'orderId' is set
            if (params.orderId == null) {
                throw new Error('Missing required parameter orderId when calling getOrderById');
            }
            let fetchParams = {
                method: 'GET',
                headers: headerParams,
                                
            };

            if (extraFetchParams) {
                fetchParams = assign(fetchParams, extraFetchParams);
            }

            let localVarPathWithQueryParameters = localVarPath + (localVarPath.indexOf('?') !== -1 ? '&' : '?') + querystring.stringify(queryParameters);

            return fetch(localVarPathWithQueryParameters, fetchParams).then((response) => {
                if (response.status >= 200 && response.status < 300) {
                    return response.json();
                } else {
                    var error = new Error(response.statusText);
                    error['response'] = response;
                    throw error;
                }
            });
        }
        /**
         * Place an order for a pet
         * 
         * @param body order placed for purchasing the pet
         */
        public placeOrder (params: {  body?: Order; }, extraQueryParams?: any, extraFetchParams?: any ) : Promise<Order> {
            const localVarPath = this.basePath + '/store/order';

            let queryParameters: any = assign({}, extraQueryParams);
            let headerParams: any = assign({}, this.defaultHeaders);
            headerParams['Content-Type'] = 'application/json';

            let fetchParams = {
                method: 'POST',
                headers: headerParams,
                body: JSON.stringify(params.body),
                
            };

            if (extraFetchParams) {
                fetchParams = assign(fetchParams, extraFetchParams);
            }

            let localVarPathWithQueryParameters = localVarPath + (localVarPath.indexOf('?') !== -1 ? '&' : '?') + querystring.stringify(queryParameters);

            return fetch(localVarPathWithQueryParameters, fetchParams).then((response) => {
                if (response.status >= 200 && response.status < 300) {
                    return response.json();
                } else {
                    var error = new Error(response.statusText);
                    error['response'] = response;
                    throw error;
                }
            });
        }
    }
//}
//export namespace  {
    'use strict';

    export class UserApi {
        protected basePath = 'http://petstore.swagger.io/v2';
        public defaultHeaders : any = {};

        constructor(basePath?: string) {
            if (basePath) {
                this.basePath = basePath;
            }
        }

        /**
         * Create user
         * This can only be done by the logged in user.
         * @param body Created user object
         */
        public createUser (params: {  body?: User; }, extraQueryParams?: any, extraFetchParams?: any ) : Promise<{}> {
            const localVarPath = this.basePath + '/user';

            let queryParameters: any = assign({}, extraQueryParams);
            let headerParams: any = assign({}, this.defaultHeaders);
            headerParams['Content-Type'] = 'application/json';

            let fetchParams = {
                method: 'POST',
                headers: headerParams,
                body: JSON.stringify(params.body),
                
            };

            if (extraFetchParams) {
                fetchParams = assign(fetchParams, extraFetchParams);
            }

            let localVarPathWithQueryParameters = localVarPath + (localVarPath.indexOf('?') !== -1 ? '&' : '?') + querystring.stringify(queryParameters);

            return fetch(localVarPathWithQueryParameters, fetchParams).then((response) => {
                if (response.status >= 200 && response.status < 300) {
                    return response.json();
                } else {
                    var error = new Error(response.statusText);
                    error['response'] = response;
                    throw error;
                }
            });
        }
        /**
         * Creates list of users with given input array
         * 
         * @param body List of user object
         */
        public createUsersWithArrayInput (params: {  body?: Array<User>; }, extraQueryParams?: any, extraFetchParams?: any ) : Promise<{}> {
            const localVarPath = this.basePath + '/user/createWithArray';

            let queryParameters: any = assign({}, extraQueryParams);
            let headerParams: any = assign({}, this.defaultHeaders);
            headerParams['Content-Type'] = 'application/json';

            let fetchParams = {
                method: 'POST',
                headers: headerParams,
                body: JSON.stringify(params.body),
                
            };

            if (extraFetchParams) {
                fetchParams = assign(fetchParams, extraFetchParams);
            }

            let localVarPathWithQueryParameters = localVarPath + (localVarPath.indexOf('?') !== -1 ? '&' : '?') + querystring.stringify(queryParameters);

            return fetch(localVarPathWithQueryParameters, fetchParams).then((response) => {
                if (response.status >= 200 && response.status < 300) {
                    return response.json();
                } else {
                    var error = new Error(response.statusText);
                    error['response'] = response;
                    throw error;
                }
            });
        }
        /**
         * Creates list of users with given input array
         * 
         * @param body List of user object
         */
        public createUsersWithListInput (params: {  body?: Array<User>; }, extraQueryParams?: any, extraFetchParams?: any ) : Promise<{}> {
            const localVarPath = this.basePath + '/user/createWithList';

            let queryParameters: any = assign({}, extraQueryParams);
            let headerParams: any = assign({}, this.defaultHeaders);
            headerParams['Content-Type'] = 'application/json';

            let fetchParams = {
                method: 'POST',
                headers: headerParams,
                body: JSON.stringify(params.body),
                
            };

            if (extraFetchParams) {
                fetchParams = assign(fetchParams, extraFetchParams);
            }

            let localVarPathWithQueryParameters = localVarPath + (localVarPath.indexOf('?') !== -1 ? '&' : '?') + querystring.stringify(queryParameters);

            return fetch(localVarPathWithQueryParameters, fetchParams).then((response) => {
                if (response.status >= 200 && response.status < 300) {
                    return response.json();
                } else {
                    var error = new Error(response.statusText);
                    error['response'] = response;
                    throw error;
                }
            });
        }
        /**
         * Delete user
         * This can only be done by the logged in user.
         * @param username The name that needs to be deleted
         */
        public deleteUser (params: {  username: string; }, extraQueryParams?: any, extraFetchParams?: any ) : Promise<{}> {
            const localVarPath = this.basePath + '/user/{username}'
                .replace('{' + 'username' + '}', String(params.username));

            let queryParameters: any = assign({}, extraQueryParams);
            let headerParams: any = assign({}, this.defaultHeaders);
            // verify required parameter 'username' is set
            if (params.username == null) {
                throw new Error('Missing required parameter username when calling deleteUser');
            }
            let fetchParams = {
                method: 'DELETE',
                headers: headerParams,
                                
            };

            if (extraFetchParams) {
                fetchParams = assign(fetchParams, extraFetchParams);
            }

            let localVarPathWithQueryParameters = localVarPath + (localVarPath.indexOf('?') !== -1 ? '&' : '?') + querystring.stringify(queryParameters);

            return fetch(localVarPathWithQueryParameters, fetchParams).then((response) => {
                if (response.status >= 200 && response.status < 300) {
                    return response.json();
                } else {
                    var error = new Error(response.statusText);
                    error['response'] = response;
                    throw error;
                }
            });
        }
        /**
         * Get user by user name
         * 
         * @param username The name that needs to be fetched. Use user1 for testing. 
         */
        public getUserByName (params: {  username: string; }, extraQueryParams?: any, extraFetchParams?: any ) : Promise<User> {
            const localVarPath = this.basePath + '/user/{username}'
                .replace('{' + 'username' + '}', String(params.username));

            let queryParameters: any = assign({}, extraQueryParams);
            let headerParams: any = assign({}, this.defaultHeaders);
            // verify required parameter 'username' is set
            if (params.username == null) {
                throw new Error('Missing required parameter username when calling getUserByName');
            }
            let fetchParams = {
                method: 'GET',
                headers: headerParams,
                                
            };

            if (extraFetchParams) {
                fetchParams = assign(fetchParams, extraFetchParams);
            }

            let localVarPathWithQueryParameters = localVarPath + (localVarPath.indexOf('?') !== -1 ? '&' : '?') + querystring.stringify(queryParameters);

            return fetch(localVarPathWithQueryParameters, fetchParams).then((response) => {
                if (response.status >= 200 && response.status < 300) {
                    return response.json();
                } else {
                    var error = new Error(response.statusText);
                    error['response'] = response;
                    throw error;
                }
            });
        }
        /**
         * Logs user into the system
         * 
         * @param username The user name for login
         * @param password The password for login in clear text
         */
        public loginUser (params: {  username?: string; password?: string; }, extraQueryParams?: any, extraFetchParams?: any ) : Promise<string> {
            const localVarPath = this.basePath + '/user/login';

            let queryParameters: any = assign({}, extraQueryParams);
            let headerParams: any = assign({}, this.defaultHeaders);
            if (params.username !== undefined) {
                queryParameters['username'] = params.username;
            }

            if (params.password !== undefined) {
                queryParameters['password'] = params.password;
            }

            let fetchParams = {
                method: 'GET',
                headers: headerParams,
                                
            };

            if (extraFetchParams) {
                fetchParams = assign(fetchParams, extraFetchParams);
            }

            let localVarPathWithQueryParameters = localVarPath + (localVarPath.indexOf('?') !== -1 ? '&' : '?') + querystring.stringify(queryParameters);

            return fetch(localVarPathWithQueryParameters, fetchParams).then((response) => {
                if (response.status >= 200 && response.status < 300) {
                    return response.json();
                } else {
                    var error = new Error(response.statusText);
                    error['response'] = response;
                    throw error;
                }
            });
        }
        /**
         * Logs out current logged in user session
         * 
         */
        public logoutUser (params: {  }, extraQueryParams?: any, extraFetchParams?: any ) : Promise<{}> {
            const localVarPath = this.basePath + '/user/logout';

            let queryParameters: any = assign({}, extraQueryParams);
            let headerParams: any = assign({}, this.defaultHeaders);
            let fetchParams = {
                method: 'GET',
                headers: headerParams,
                                
            };

            if (extraFetchParams) {
                fetchParams = assign(fetchParams, extraFetchParams);
            }

            let localVarPathWithQueryParameters = localVarPath + (localVarPath.indexOf('?') !== -1 ? '&' : '?') + querystring.stringify(queryParameters);

            return fetch(localVarPathWithQueryParameters, fetchParams).then((response) => {
                if (response.status >= 200 && response.status < 300) {
                    return response.json();
                } else {
                    var error = new Error(response.statusText);
                    error['response'] = response;
                    throw error;
                }
            });
        }
        /**
         * Updated user
         * This can only be done by the logged in user.
         * @param username name that need to be deleted
         * @param body Updated user object
         */
        public updateUser (params: {  username: string; body?: User; }, extraQueryParams?: any, extraFetchParams?: any ) : Promise<{}> {
            const localVarPath = this.basePath + '/user/{username}'
                .replace('{' + 'username' + '}', String(params.username));

            let queryParameters: any = assign({}, extraQueryParams);
            let headerParams: any = assign({}, this.defaultHeaders);
            headerParams['Content-Type'] = 'application/json';

            // verify required parameter 'username' is set
            if (params.username == null) {
                throw new Error('Missing required parameter username when calling updateUser');
            }
            let fetchParams = {
                method: 'PUT',
                headers: headerParams,
                body: JSON.stringify(params.body),
                
            };

            if (extraFetchParams) {
                fetchParams = assign(fetchParams, extraFetchParams);
            }

            let localVarPathWithQueryParameters = localVarPath + (localVarPath.indexOf('?') !== -1 ? '&' : '?') + querystring.stringify(queryParameters);

            return fetch(localVarPathWithQueryParameters, fetchParams).then((response) => {
                if (response.status >= 200 && response.status < 300) {
                    return response.json();
                } else {
                    var error = new Error(response.statusText);
                    error['response'] = response;
                    throw error;
                }
            });
        }
    }
//}
