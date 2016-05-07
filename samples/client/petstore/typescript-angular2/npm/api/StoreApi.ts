import {Http, Headers, RequestOptionsArgs, Response, URLSearchParams} from 'angular2/http';
import {Injectable} from 'angular2/core';
import {Observable} from 'rxjs/Observable';
import * as models from '../model/models';

/* tslint:disable:no-unused-variable member-ordering */

'use strict';

@Injectable()
export class StoreApi {
    protected basePath = 'http://petstore.swagger.io/v2';
    public defaultHeaders : Headers = new Headers();

    constructor(protected http: Http, basePath: string) {
        if (basePath) {
            this.basePath = basePath;
        }
    }

    /**
     * Delete purchase order by ID
     * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
     * @param orderId ID of the order that needs to be deleted
     */
    public deleteOrder (orderId: string, extraHttpRequestParams?: any ) : Observable<{}> {
        const path = this.basePath + '/store/order/{orderId}'
            .replace('{' + 'orderId' + '}', String(orderId));

        let queryParameters: any = ""; // This should probably be an object in the future
        let headerParams = this.defaultHeaders;
        // verify required parameter 'orderId' is set
        if (!orderId) {
            throw new Error('Missing required parameter orderId when calling deleteOrder');
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
     * Returns pet inventories by status
     * Returns a map of status codes to quantities
     */
    public getInventory (extraHttpRequestParams?: any ) : Observable<{ [key: string]: number; }> {
        const path = this.basePath + '/store/inventory';

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
     * Find purchase order by ID
     * For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
     * @param orderId ID of pet that needs to be fetched
     */
    public getOrderById (orderId: string, extraHttpRequestParams?: any ) : Observable<models.Order> {
        const path = this.basePath + '/store/order/{orderId}'
            .replace('{' + 'orderId' + '}', String(orderId));

        let queryParameters: any = ""; // This should probably be an object in the future
        let headerParams = this.defaultHeaders;
        // verify required parameter 'orderId' is set
        if (!orderId) {
            throw new Error('Missing required parameter orderId when calling getOrderById');
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
     * Place an order for a pet
     * 
     * @param body order placed for purchasing the pet
     */
    public placeOrder (body?: models.Order, extraHttpRequestParams?: any ) : Observable<models.Order> {
        const path = this.basePath + '/store/order';

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

}
