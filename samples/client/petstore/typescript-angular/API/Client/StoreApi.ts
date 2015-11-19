/// <reference path="api.d.ts" />

/* tslint:disable:no-unused-variable member-ordering */

namespace API.Client {
    'use strict';

    export class StoreApi {
        protected basePath = 'http://petstore.swagger.io/v2';
        public defaultHeaders : any = {};

        static $inject: string[] = ['$http', '$httpParamSerializer'];

        constructor(protected $http: ng.IHttpService, protected $httpParamSerializer?: (d: any) => any, basePath?: string) {
            if (basePath) {
                this.basePath = basePath;
            }
        }

        private extendObj<T1,T2>(objA: T1, objB: T2) {
            for(let key in objB){
                if(objB.hasOwnProperty(key)){
                    objA[key] = objB[key];
                }
            }
            return <T1&T2>objA;
        }

        /**
         * Returns pet inventories by status
         * Returns a map of status codes to quantities
         */
        public getInventory (extraHttpRequestParams?: any ) : ng.IHttpPromise<{ [key: string]: number; }> {
            const path = this.basePath + '/store/inventory';

            let queryParameters: any = {};
            let headerParams: any = this.extendObj({}, this.defaultHeaders);
            let httpRequestParams: any = {
                method: 'GET',
                url: path,
                json: true,
                
                
                params: queryParameters,
                headers: headerParams
            };

            if (extraHttpRequestParams) {
                httpRequestParams = this.extendObj(httpRequestParams, extraHttpRequestParams);
            }

            return this.$http(httpRequestParams);
        }
        /**
         * Place an order for a pet
         * 
         * @param body order placed for purchasing the pet
         */
        public placeOrder (body?: Order, extraHttpRequestParams?: any ) : ng.IHttpPromise<Order> {
            const path = this.basePath + '/store/order';

            let queryParameters: any = {};
            let headerParams: any = this.extendObj({}, this.defaultHeaders);
            let httpRequestParams: any = {
                method: 'POST',
                url: path,
                json: true,
                data: body,
                
                
                params: queryParameters,
                headers: headerParams
            };

            if (extraHttpRequestParams) {
                httpRequestParams = this.extendObj(httpRequestParams, extraHttpRequestParams);
            }

            return this.$http(httpRequestParams);
        }
        /**
         * Find purchase order by ID
         * For valid response try integer IDs with value &lt;= 5 or &gt; 10. Other values will generated exceptions
         * @param orderId ID of pet that needs to be fetched
         */
        public getOrderById (orderId: string, extraHttpRequestParams?: any ) : ng.IHttpPromise<Order> {
            const path = this.basePath + '/store/order/{orderId}'
                .replace('{' + 'orderId' + '}', String(orderId));

            let queryParameters: any = {};
            let headerParams: any = this.extendObj({}, this.defaultHeaders);
            // verify required parameter 'orderId' is set
            if (!orderId) {
                throw new Error('Missing required parameter orderId when calling getOrderById');
            }
            let httpRequestParams: any = {
                method: 'GET',
                url: path,
                json: true,
                
                
                params: queryParameters,
                headers: headerParams
            };

            if (extraHttpRequestParams) {
                httpRequestParams = this.extendObj(httpRequestParams, extraHttpRequestParams);
            }

            return this.$http(httpRequestParams);
        }
        /**
         * Delete purchase order by ID
         * For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
         * @param orderId ID of the order that needs to be deleted
         */
        public deleteOrder (orderId: string, extraHttpRequestParams?: any ) : ng.IHttpPromise<{}> {
            const path = this.basePath + '/store/order/{orderId}'
                .replace('{' + 'orderId' + '}', String(orderId));

            let queryParameters: any = {};
            let headerParams: any = this.extendObj({}, this.defaultHeaders);
            // verify required parameter 'orderId' is set
            if (!orderId) {
                throw new Error('Missing required parameter orderId when calling deleteOrder');
            }
            let httpRequestParams: any = {
                method: 'DELETE',
                url: path,
                json: true,
                
                
                params: queryParameters,
                headers: headerParams
            };

            if (extraHttpRequestParams) {
                httpRequestParams = this.extendObj(httpRequestParams, extraHttpRequestParams);
            }

            return this.$http(httpRequestParams);
        }
    }
}
