import {Http, Headers, RequestOptionsArgs, Response, URLSearchParams} from '@angular/http';
import {Injectable, Optional} from '@angular/core';
import {Observable} from 'rxjs/Observable';
import * as models from '../model/models';
import 'rxjs/Rx';

/* tslint:disable:no-unused-variable member-ordering */

'use strict';

@Injectable()
export class ProjectApi {
    protected basePath = 'https://localhost/v1';
    public defaultHeaders : Headers = new Headers();

    constructor(protected http: Http, @Optional() basePath: string) {
        if (basePath) {
            this.basePath = basePath;
        }
    }

    /**
     * Create a Project
     * Creates an empty Project
     * @param name 
     * @param address 
     * @param longitude 
     * @param latitude 
     * @param meta 
     */
    public createProject (name?: string, address?: string, longitude?: number, latitude?: number, meta?: string, extraHttpRequestParams?: any ) : Observable<models.ProjectEntity> {
        const path = this.basePath + '/projects';

        let queryParameters: any = ""; // This should probably be an object in the future
        let headerParams = this.defaultHeaders;
        let formParams = new URLSearchParams();

        headerParams.set('Content-Type', 'application/x-www-form-urlencoded');

        formParams['name'] = name;

        formParams['address'] = address;

        formParams['longitude'] = longitude;

        formParams['latitude'] = latitude;

        formParams['meta'] = meta;

        let requestOptions: RequestOptionsArgs = {
            method: 'POST',
            headers: headerParams,
            search: queryParameters
        };
        requestOptions.body = formParams.toString();

        return this.http.request(path, requestOptions)
            .map((response: Response) => response.json());
    }

    /**
     * Delete a Project
     * Returns a Project JSON object
     * @param id Project id
     */
    public deleteProjectById (id: number, extraHttpRequestParams?: any ) : Observable<{}> {
        const path = this.basePath + '/projects/{id}'
            .replace('{' + 'id' + '}', String(id));

        let queryParameters: any = ""; // This should probably be an object in the future
        let headerParams = this.defaultHeaders;
        // verify required parameter 'id' is set
        if (!id) {
            throw new Error('Missing required parameter id when calling deleteProjectById');
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
     * Get a Project
     * Returns a Project JSON object
     * @param id Project id
     */
    public getProjectById (id: number, extraHttpRequestParams?: any ) : Observable<models.ProjectEntity> {
        const path = this.basePath + '/projects/{id}'
            .replace('{' + 'id' + '}', String(id));

        let queryParameters: any = ""; // This should probably be an object in the future
        let headerParams = this.defaultHeaders;
        // verify required parameter 'id' is set
        if (!id) {
            throw new Error('Missing required parameter id when calling getProjectById');
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
     * Get project list
     * Returns a Project JSON object
     * @param page 
     * @param perPage 
     * @param kind 
     * @param q 
     * @param filter 
     * @param latitude Valid with kind as location
     * @param longitude Valid with kind as location
     * @param scope Valid with kind as location, and between 1~9
     */
    public getProjectList (page?: number, perPage?: number, kind?: string, q?: string, filter?: string, latitude?: number, longitude?: number, scope?: number, extraHttpRequestParams?: any ) : Observable<models.ProjectList> {
        const path = this.basePath + '/projects';

        let queryParameters: any = ""; // This should probably be an object in the future
        let headerParams = this.defaultHeaders;
        if (page !== undefined) {
            queryParameters['page'] = page;
        }

        if (perPage !== undefined) {
            queryParameters['per_page'] = perPage;
        }

        if (kind !== undefined) {
            queryParameters['kind'] = kind;
        }

        if (q !== undefined) {
            queryParameters['q'] = q;
        }

        if (filter !== undefined) {
            queryParameters['filter'] = filter;
        }

        if (latitude !== undefined) {
            queryParameters['latitude'] = latitude;
        }

        if (longitude !== undefined) {
            queryParameters['longitude'] = longitude;
        }

        if (scope !== undefined) {
            queryParameters['scope'] = scope;
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
     * Update project
     * 
     * @param id Project id
     * @param name User ID
     * @param address Address
     * @param longitude 
     * @param latitude 
     * @param meta 
     * @param thumbnail Project thumbnail
     */
    public updateProject (id: number, name?: string, address?: string, longitude?: number, latitude?: number, meta?: string, thumbnail?: any, extraHttpRequestParams?: any ) : Observable<models.ProjectEntity> {
        const path = this.basePath + '/projects/{id}'
            .replace('{' + 'id' + '}', String(id));

        let queryParameters: any = ""; // This should probably be an object in the future
        let headerParams = this.defaultHeaders;
        let formParams = new URLSearchParams();

        // verify required parameter 'id' is set
        if (!id) {
            throw new Error('Missing required parameter id when calling updateProject');
        }
        headerParams.set('Content-Type', 'application/x-www-form-urlencoded');

        formParams['name'] = name;

        formParams['address'] = address;

        formParams['longitude'] = longitude;

        formParams['latitude'] = latitude;

        formParams['meta'] = meta;

        formParams['thumbnail'] = thumbnail;

        let requestOptions: RequestOptionsArgs = {
            method: 'PUT',
            headers: headerParams,
            search: queryParameters
        };
        requestOptions.body = formParams.toString();

        return this.http.request(path, requestOptions)
            .map((response: Response) => response.json());
    }

}
