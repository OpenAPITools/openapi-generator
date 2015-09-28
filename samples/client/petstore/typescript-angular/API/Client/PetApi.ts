/// <reference path="api.d.ts" />

/* tslint:disable:no-unused-variable member-ordering */

module API.Client {
    'use strict';

    export class PetApi {
        private basePath = 'http://petstore.swagger.io/v2';

        static $inject: string[] = ['$http', '$httpParamSerializer'];

        constructor(private $http: ng.IHttpService, basePath?: string, private $httpParamSerializer?: (any) => any) {
            if (basePath) {
                this.basePath = basePath;
            }
        }

        public updatePet (body?: Pet, extraHttpRequestParams?: any ) : ng.IHttpPromise<{}> {
            var path = this.basePath + '/pet';

            var queryParameters: any = {};
            var headerParams: any = {};
            var httpRequestParams: any = {
                method: 'PUT',
                url: path,
                json: true,
                data: body,
                
                
                params: queryParameters,
                headers: headerParams
            };

            if (extraHttpRequestParams) {
                for (var k in extraHttpRequestParams) {
                    if (extraHttpRequestParams.hasOwnProperty(k)) {
                        httpRequestParams[k] = extraHttpRequestParams[k];
                    }
                }
            }

            return this.$http(httpRequestParams);
        }

        public addPet (body?: Pet, extraHttpRequestParams?: any ) : ng.IHttpPromise<{}> {
            var path = this.basePath + '/pet';

            var queryParameters: any = {};
            var headerParams: any = {};
            var httpRequestParams: any = {
                method: 'POST',
                url: path,
                json: true,
                data: body,
                
                
                params: queryParameters,
                headers: headerParams
            };

            if (extraHttpRequestParams) {
                for (var k in extraHttpRequestParams) {
                    if (extraHttpRequestParams.hasOwnProperty(k)) {
                        httpRequestParams[k] = extraHttpRequestParams[k];
                    }
                }
            }

            return this.$http(httpRequestParams);
        }

        public findPetsByStatus (status?: Array<string>, extraHttpRequestParams?: any ) : ng.IHttpPromise<Array<Pet>> {
            var path = this.basePath + '/pet/findByStatus';

            var queryParameters: any = {};
            var headerParams: any = {};
            if (status !== undefined) {
                queryParameters['status'] = status;
            }

            var httpRequestParams: any = {
                method: 'GET',
                url: path,
                json: true,
                
                
                params: queryParameters,
                headers: headerParams
            };

            if (extraHttpRequestParams) {
                for (var k in extraHttpRequestParams) {
                    if (extraHttpRequestParams.hasOwnProperty(k)) {
                        httpRequestParams[k] = extraHttpRequestParams[k];
                    }
                }
            }

            return this.$http(httpRequestParams);
        }

        public findPetsByTags (tags?: Array<string>, extraHttpRequestParams?: any ) : ng.IHttpPromise<Array<Pet>> {
            var path = this.basePath + '/pet/findByTags';

            var queryParameters: any = {};
            var headerParams: any = {};
            if (tags !== undefined) {
                queryParameters['tags'] = tags;
            }

            var httpRequestParams: any = {
                method: 'GET',
                url: path,
                json: true,
                
                
                params: queryParameters,
                headers: headerParams
            };

            if (extraHttpRequestParams) {
                for (var k in extraHttpRequestParams) {
                    if (extraHttpRequestParams.hasOwnProperty(k)) {
                        httpRequestParams[k] = extraHttpRequestParams[k];
                    }
                }
            }

            return this.$http(httpRequestParams);
        }

        public getPetById (petId: number, extraHttpRequestParams?: any ) : ng.IHttpPromise<Pet> {
            var path = this.basePath + '/pet/{petId}';

            path = path.replace('{' + 'petId' + '}', String(petId));

            var queryParameters: any = {};
            var headerParams: any = {};
            // verify required parameter 'petId' is set
            if (!petId) {
                throw new Error('Missing required parameter petId when calling getPetById');
            }

            var httpRequestParams: any = {
                method: 'GET',
                url: path,
                json: true,
                
                
                params: queryParameters,
                headers: headerParams
            };

            if (extraHttpRequestParams) {
                for (var k in extraHttpRequestParams) {
                    if (extraHttpRequestParams.hasOwnProperty(k)) {
                        httpRequestParams[k] = extraHttpRequestParams[k];
                    }
                }
            }

            return this.$http(httpRequestParams);
        }

        public updatePetWithForm (petId: string, name?: string, status?: string, extraHttpRequestParams?: any ) : ng.IHttpPromise<{}> {
            var path = this.basePath + '/pet/{petId}';

            path = path.replace('{' + 'petId' + '}', String(petId));

            var queryParameters: any = {};
            var headerParams: any = {};
            var formParams: any = {};

            // verify required parameter 'petId' is set
            if (!petId) {
                throw new Error('Missing required parameter petId when calling updatePetWithForm');
            }

            headerParams['Content-Type'] = 'application/x-www-form-urlencoded';

            formParams['name'] = name;
            
            formParams['status'] = status;
            
            var httpRequestParams: any = {
                method: 'POST',
                url: path,
                json: false,
                
                data: this.$httpParamSerializer(formParams),
                
                params: queryParameters,
                headers: headerParams
            };

            if (extraHttpRequestParams) {
                for (var k in extraHttpRequestParams) {
                    if (extraHttpRequestParams.hasOwnProperty(k)) {
                        httpRequestParams[k] = extraHttpRequestParams[k];
                    }
                }
            }

            return this.$http(httpRequestParams);
        }

        public deletePet (petId: number, apiKey?: string, extraHttpRequestParams?: any ) : ng.IHttpPromise<{}> {
            var path = this.basePath + '/pet/{petId}';

            path = path.replace('{' + 'petId' + '}', String(petId));

            var queryParameters: any = {};
            var headerParams: any = {};
            // verify required parameter 'petId' is set
            if (!petId) {
                throw new Error('Missing required parameter petId when calling deletePet');
            }

            headerParams['api_key'] = apiKey;

            var httpRequestParams: any = {
                method: 'DELETE',
                url: path,
                json: true,
                
                
                params: queryParameters,
                headers: headerParams
            };

            if (extraHttpRequestParams) {
                for (var k in extraHttpRequestParams) {
                    if (extraHttpRequestParams.hasOwnProperty(k)) {
                        httpRequestParams[k] = extraHttpRequestParams[k];
                    }
                }
            }

            return this.$http(httpRequestParams);
        }

        public uploadFile (petId: number, additionalMetadata?: string, file?: any, extraHttpRequestParams?: any ) : ng.IHttpPromise<{}> {
            var path = this.basePath + '/pet/{petId}/uploadImage';

            path = path.replace('{' + 'petId' + '}', String(petId));

            var queryParameters: any = {};
            var headerParams: any = {};
            var formParams: any = {};

            // verify required parameter 'petId' is set
            if (!petId) {
                throw new Error('Missing required parameter petId when calling uploadFile');
            }

            headerParams['Content-Type'] = 'application/x-www-form-urlencoded';

            formParams['additionalMetadata'] = additionalMetadata;
            
            formParams['file'] = file;
            
            var httpRequestParams: any = {
                method: 'POST',
                url: path,
                json: false,
                
                data: this.$httpParamSerializer(formParams),
                
                params: queryParameters,
                headers: headerParams
            };

            if (extraHttpRequestParams) {
                for (var k in extraHttpRequestParams) {
                    if (extraHttpRequestParams.hasOwnProperty(k)) {
                        httpRequestParams[k] = extraHttpRequestParams[k];
                    }
                }
            }

            return this.$http(httpRequestParams);
        }
    }
}
