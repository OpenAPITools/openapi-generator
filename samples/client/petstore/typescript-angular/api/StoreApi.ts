/// <reference path="api.d.ts" />

/* tslint:disable:no-unused-variable member-ordering */

module api {
    'use strict';

    
    export class StoreApi {
        private basePath = 'http://petstore.swagger.io/v2';

        static $inject: string[] = ['$http'];

        constructor(private $http: ng.IHttpService, basePath?: string) {
            if (basePath) {
                this.basePath = basePath;
            }
        }
        
        public getInventory ( extraHttpRequestParams?: any ) : ng.IHttpPromise<map<String, number>> {
            var path = this.basePath + '/store/inventory';
            
            var queryParameters: any = {};
            var headers: any = {};
            
            
            
            var httpRequestParams: any = {
                method: 'GET',
                url: path,
                json: true,
                
                params: queryParameters,
                headers: headers
            };

            if (extraHttpRequestParams) {
                for (var k in extraHttpRequestParams){
                    if (extraHttpRequestParams.hasOwnProperty(k)) {
                        httpRequestParams[k] = extraHttpRequestParams[k];
                    }
                }
            }

            return this.$http(httpRequestParams);
        }
		
        public placeOrder (body: Order,  extraHttpRequestParams?: any ) : ng.IHttpPromise<Order> {
            var path = this.basePath + '/store/order';
            
            var queryParameters: any = {};
            var headers: any = {};
            
            
            
            var httpRequestParams: any = {
                method: 'POST',
                url: path,
                json: true,
                data: body,
                
                params: queryParameters,
                headers: headers
            };

            if (extraHttpRequestParams) {
                for (var k in extraHttpRequestParams){
                    if (extraHttpRequestParams.hasOwnProperty(k)) {
                        httpRequestParams[k] = extraHttpRequestParams[k];
                    }
                }
            }

            return this.$http(httpRequestParams);
        }
		
        public getOrderById (orderId: string,  extraHttpRequestParams?: any ) : ng.IHttpPromise<Order> {
            var path = this.basePath + '/store/order/{orderId}';
            
            path = path.replace('{' + 'orderId' + '}', String(orderId));
            
            var queryParameters: any = {};
            var headers: any = {};
            
            // verify required parameter 'orderId' is set
            if (!orderId) {
                throw new Error('Missing required parameter orderId when calling getOrderById');
            }
            
            
            
            var httpRequestParams: any = {
                method: 'GET',
                url: path,
                json: true,
                
                params: queryParameters,
                headers: headers
            };

            if (extraHttpRequestParams) {
                for (var k in extraHttpRequestParams){
                    if (extraHttpRequestParams.hasOwnProperty(k)) {
                        httpRequestParams[k] = extraHttpRequestParams[k];
                    }
                }
            }

            return this.$http(httpRequestParams);
        }
		
        public deleteOrder (orderId: string,  extraHttpRequestParams?: any ) : ng.IHttpPromise<{}> {
            var path = this.basePath + '/store/order/{orderId}';
            
            path = path.replace('{' + 'orderId' + '}', String(orderId));
            
            var queryParameters: any = {};
            var headers: any = {};
            
            // verify required parameter 'orderId' is set
            if (!orderId) {
                throw new Error('Missing required parameter orderId when calling deleteOrder');
            }
            
            
            
            var httpRequestParams: any = {
                method: 'DELETE',
                url: path,
                json: true,
                
                params: queryParameters,
                headers: headers
            };

            if (extraHttpRequestParams) {
                for (var k in extraHttpRequestParams){
                    if (extraHttpRequestParams.hasOwnProperty(k)) {
                        httpRequestParams[k] = extraHttpRequestParams[k];
                    }
                }
            }

            return this.$http(httpRequestParams);
        }
		
    }

    angular.module('api_StoreApi', ['$http'])
        .service('StoreApi', StoreApi);
}
