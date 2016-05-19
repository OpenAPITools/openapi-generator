import {Http, Headers, RequestOptionsArgs, Response, URLSearchParams} from '@angular/http';
import {Injectable, Optional} from '@angular/core';
import {Observable} from 'rxjs/Observable';
import * as models from '../model/models';
import 'rxjs/Rx';

/* tslint:disable:no-unused-variable member-ordering */

'use strict';

@Injectable()
export class UserApi {
    protected basePath = 'http://additional-properties.swagger.io/v2';
    public defaultHeaders : Headers = new Headers();

    constructor(protected http: Http, @Optional() basePath: string) {
        if (basePath) {
            this.basePath = basePath;
        }
    }

    /**
     * Add a new User to the store
     * 
     * @param body User object that needs to be added to the store
     */
    public addUser (body?: models.User, extraHttpRequestParams?: any ) : Observable<{}> {
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
     * Update an existing User
     * 
     * @param body User object that needs to be added to the store
     */
    public updateUser (body?: models.User, extraHttpRequestParams?: any ) : Observable<{}> {
        const path = this.basePath + '/user';

        let queryParameters: any = ""; // This should probably be an object in the future
        let headerParams = this.defaultHeaders;
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
