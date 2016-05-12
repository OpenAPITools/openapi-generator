/// <reference path="api.d.ts" />

/* tslint:disable:no-unused-variable member-ordering */

namespace API.Client {
    'use strict';

    export class PetApi {
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
         * Add a new pet to the store
         * 
         * @param body Pet object that needs to be added to the store
         */
        public addPet (body?: Pet, extraHttpRequestParams?: any ) : ng.IHttpPromise<{}> {
            const localVarPath = this.basePath + '/pet';

            let queryParameters: any = {};
            let headerParams: any = this.extendObj({}, this.defaultHeaders);
            let httpRequestParams: any = {
                method: 'POST',
                url: localVarPath,
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
         * Deletes a pet
         * 
         * @param petId Pet id to delete
         * @param apiKey 
         */
        public deletePet (petId: number, apiKey?: string, extraHttpRequestParams?: any ) : ng.IHttpPromise<{}> {
            const localVarPath = this.basePath + '/pet/{petId}'
                .replace('{' + 'petId' + '}', String(petId));

            let queryParameters: any = {};
            let headerParams: any = this.extendObj({}, this.defaultHeaders);
            // verify required parameter 'petId' is set
            if (!petId) {
                throw new Error('Missing required parameter petId when calling deletePet');
            }
            headerParams['api_key'] = apiKey;

            let httpRequestParams: any = {
                method: 'DELETE',
                url: localVarPath,
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
         * Finds Pets by status
         * Multiple status values can be provided with comma seperated strings
         * @param status Status values that need to be considered for filter
         */
        public findPetsByStatus (status?: Array<string>, extraHttpRequestParams?: any ) : ng.IHttpPromise<Array<Pet>> {
            const localVarPath = this.basePath + '/pet/findByStatus';

            let queryParameters: any = {};
            let headerParams: any = this.extendObj({}, this.defaultHeaders);
            if (status !== undefined) {
                queryParameters['status'] = status;
            }

            let httpRequestParams: any = {
                method: 'GET',
                url: localVarPath,
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
         * Finds Pets by tags
         * Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
         * @param tags Tags to filter by
         */
        public findPetsByTags (tags?: Array<string>, extraHttpRequestParams?: any ) : ng.IHttpPromise<Array<Pet>> {
            const localVarPath = this.basePath + '/pet/findByTags';

            let queryParameters: any = {};
            let headerParams: any = this.extendObj({}, this.defaultHeaders);
            if (tags !== undefined) {
                queryParameters['tags'] = tags;
            }

            let httpRequestParams: any = {
                method: 'GET',
                url: localVarPath,
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
         * Find pet by ID
         * Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
         * @param petId ID of pet that needs to be fetched
         */
        public getPetById (petId: number, extraHttpRequestParams?: any ) : ng.IHttpPromise<Pet> {
            const localVarPath = this.basePath + '/pet/{petId}'
                .replace('{' + 'petId' + '}', String(petId));

            let queryParameters: any = {};
            let headerParams: any = this.extendObj({}, this.defaultHeaders);
            // verify required parameter 'petId' is set
            if (!petId) {
                throw new Error('Missing required parameter petId when calling getPetById');
            }
            let httpRequestParams: any = {
                method: 'GET',
                url: localVarPath,
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
         * Update an existing pet
         * 
         * @param body Pet object that needs to be added to the store
         */
        public updatePet (body?: Pet, extraHttpRequestParams?: any ) : ng.IHttpPromise<{}> {
            const localVarPath = this.basePath + '/pet';

            let queryParameters: any = {};
            let headerParams: any = this.extendObj({}, this.defaultHeaders);
            let httpRequestParams: any = {
                method: 'PUT',
                url: localVarPath,
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
         * Updates a pet in the store with form data
         * 
         * @param petId ID of pet that needs to be updated
         * @param name Updated name of the pet
         * @param status Updated status of the pet
         */
        public updatePetWithForm (petId: string, name?: string, status?: string, extraHttpRequestParams?: any ) : ng.IHttpPromise<{}> {
            const localVarPath = this.basePath + '/pet/{petId}'
                .replace('{' + 'petId' + '}', String(petId));

            let queryParameters: any = {};
            let headerParams: any = this.extendObj({}, this.defaultHeaders);
            let formParams: any = {};

            // verify required parameter 'petId' is set
            if (!petId) {
                throw new Error('Missing required parameter petId when calling updatePetWithForm');
            }
            headerParams['Content-Type'] = 'application/x-www-form-urlencoded';

            formParams['name'] = name;

            formParams['status'] = status;

            let httpRequestParams: any = {
                method: 'POST',
                url: localVarPath,
                json: false,
                                data: this.$httpParamSerializer(formParams),
                params: queryParameters,
                headers: headerParams
            };

            if (extraHttpRequestParams) {
                httpRequestParams = this.extendObj(httpRequestParams, extraHttpRequestParams);
            }

            return this.$http(httpRequestParams);
        }
        /**
         * uploads an image
         * 
         * @param petId ID of pet to update
         * @param additionalMetadata Additional data to pass to server
         * @param file file to upload
         */
        public uploadFile (petId: number, additionalMetadata?: string, file?: any, extraHttpRequestParams?: any ) : ng.IHttpPromise<{}> {
            const localVarPath = this.basePath + '/pet/{petId}/uploadImage'
                .replace('{' + 'petId' + '}', String(petId));

            let queryParameters: any = {};
            let headerParams: any = this.extendObj({}, this.defaultHeaders);
            let formParams: any = {};

            // verify required parameter 'petId' is set
            if (!petId) {
                throw new Error('Missing required parameter petId when calling uploadFile');
            }
            headerParams['Content-Type'] = 'application/x-www-form-urlencoded';

            formParams['additionalMetadata'] = additionalMetadata;

            formParams['file'] = file;

            let httpRequestParams: any = {
                method: 'POST',
                url: localVarPath,
                json: false,
                                data: this.$httpParamSerializer(formParams),
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
