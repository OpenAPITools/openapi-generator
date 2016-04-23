import {Http, Headers, RequestOptionsArgs, Response, URLSearchParams} from 'angular2/http';
import {Injectable} from 'angular2/core';
import {Observable} from 'rxjs/Observable';
import * as models from '../model/models';

/* tslint:disable:no-unused-variable member-ordering */

'use strict';

@Injectable()
export class UserApi {
    protected basePath = 'http://petstore.swagger.io/v2';
    public defaultHeaders : Headers = new Headers();

    constructor(protected http: Http, basePath: string) {
        if (basePath) {
            this.basePath = basePath;
        }
    }

    /**
     * Create user
     * This can only be done by the logged in user.
     * @param body Created user object
     */
    public createUser (body?: models.User, extraHttpRequestParams?: any ) : Observable<{}> {
        const path = this.basePath + '/user';

        let queryParameters: any = ""; // This should probably be an object in the future
        let headerParams = this.defaultHeaders;
        let requestOptions: RequestOptionsArgs = {
            method: 'POST',
            headers: headerParams,
            search: queryParameters
        };
        requestOptions.body = JSON.stringify(body);

        return this.http.request(path, requestOptions)
            .map((response: Response) => response.json());
    }

    /**
     * Creates list of users with given input array
     * 
     * @param body List of user object
     */
    public createUsersWithArrayInput (body?: models.Array<models.User>, extraHttpRequestParams?: any ) : Observable<{}> {
        const path = this.basePath + '/user/createWithArray';

        let queryParameters: any = ""; // This should probably be an object in the future
        let headerParams = this.defaultHeaders;
        let requestOptions: RequestOptionsArgs = {
            method: 'POST',
            headers: headerParams,
            search: queryParameters
        };
        requestOptions.body = JSON.stringify(body);

        return this.http.request(path, requestOptions)
            .map((response: Response) => response.json());
    }

    /**
     * Creates list of users with given input array
     * 
     * @param body List of user object
     */
    public createUsersWithListInput (body?: models.Array<models.User>, extraHttpRequestParams?: any ) : Observable<{}> {
        const path = this.basePath + '/user/createWithList';

        let queryParameters: any = ""; // This should probably be an object in the future
        let headerParams = this.defaultHeaders;
        let requestOptions: RequestOptionsArgs = {
            method: 'POST',
            headers: headerParams,
            search: queryParameters
        };
        requestOptions.body = JSON.stringify(body);

        return this.http.request(path, requestOptions)
            .map((response: Response) => response.json());
    }

    /**
     * Delete user
     * This can only be done by the logged in user.
     * @param username The name that needs to be deleted
     */
    public deleteUser (username: string, extraHttpRequestParams?: any ) : Observable<{}> {
        const path = this.basePath + '/user/{username}'
            .replace('{' + 'username' + '}', String(username));

        let queryParameters: any = ""; // This should probably be an object in the future
        let headerParams = this.defaultHeaders;
        // verify required parameter 'username' is set
        if (!username) {
            throw new Error('Missing required parameter username when calling deleteUser');
        }
        let requestOptions: RequestOptionsArgs = {
            method: 'DELETE',
            headers: headerParams,
            search: queryParameters
        };

        return this.http.request(path, requestOptions)
            .map((response: Response) => response.json());
    }

    /**
     * Get user by user name
     * 
     * @param username The name that needs to be fetched. Use user1 for testing. 
     */
    public getUserByName (username: string, extraHttpRequestParams?: any ) : Observable<models.User> {
        const path = this.basePath + '/user/{username}'
            .replace('{' + 'username' + '}', String(username));

        let queryParameters: any = ""; // This should probably be an object in the future
        let headerParams = this.defaultHeaders;
        // verify required parameter 'username' is set
        if (!username) {
            throw new Error('Missing required parameter username when calling getUserByName');
        }
        let requestOptions: RequestOptionsArgs = {
            method: 'GET',
            headers: headerParams,
            search: queryParameters
        };

        return this.http.request(path, requestOptions)
            .map((response: Response) => response.json());
    }

    /**
     * Logs user into the system
     * 
     * @param username The user name for login
     * @param password The password for login in clear text
     */
    public loginUser (username?: string, password?: string, extraHttpRequestParams?: any ) : Observable<string> {
        const path = this.basePath + '/user/login';

        let queryParameters: any = ""; // This should probably be an object in the future
        let headerParams = this.defaultHeaders;
        if (username !== undefined) {
            queryParameters['username'] = username;
        }

        if (password !== undefined) {
            queryParameters['password'] = password;
        }

        let requestOptions: RequestOptionsArgs = {
            method: 'GET',
            headers: headerParams,
            search: queryParameters
        };

        return this.http.request(path, requestOptions)
            .map((response: Response) => response.json());
    }

    /**
     * Logs out current logged in user session
     * 
     */
    public logoutUser (extraHttpRequestParams?: any ) : Observable<{}> {
        const path = this.basePath + '/user/logout';

        let queryParameters: any = ""; // This should probably be an object in the future
        let headerParams = this.defaultHeaders;
        let requestOptions: RequestOptionsArgs = {
            method: 'GET',
            headers: headerParams,
            search: queryParameters
        };

        return this.http.request(path, requestOptions)
            .map((response: Response) => response.json());
    }

    /**
     * Updated user
     * This can only be done by the logged in user.
     * @param username name that need to be deleted
     * @param body Updated user object
     */
    public updateUser (username: string, body?: models.User, extraHttpRequestParams?: any ) : Observable<{}> {
        const path = this.basePath + '/user/{username}'
            .replace('{' + 'username' + '}', String(username));

        let queryParameters: any = ""; // This should probably be an object in the future
        let headerParams = this.defaultHeaders;
        // verify required parameter 'username' is set
        if (!username) {
            throw new Error('Missing required parameter username when calling updateUser');
        }
        let requestOptions: RequestOptionsArgs = {
            method: 'PUT',
            headers: headerParams,
            search: queryParameters
        };
        requestOptions.body = JSON.stringify(body);

        return this.http.request(path, requestOptions)
            .map((response: Response) => response.json());
    }

}
