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



        public updatePet (body?: Pet, extraHttpRequestParams?: any ) : ng.IHttpPromise<{}> {
            const path = this.basePath + '/pet';

            let queryParameters: any = {};
            let headerParams: any = this.extendObj({}, this.defaultHeaders);








            let httpRequestParams: any = {
                method: 'PUT',
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


        public addPet (body?: Pet, extraHttpRequestParams?: any ) : ng.IHttpPromise<{}> {
            const path = this.basePath + '/pet';

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


        public findPetsByStatus (status?: Array<string>, extraHttpRequestParams?: any ) : ng.IHttpPromise<Array<Pet>> {
            const path = this.basePath + '/pet/findByStatus';

            let queryParameters: any = {};
            let headerParams: any = this.extendObj({}, this.defaultHeaders);





            if (status !== undefined) {
                queryParameters['status'] = status;
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


        public findPetsByTags (tags?: Array<string>, extraHttpRequestParams?: any ) : ng.IHttpPromise<Array<Pet>> {
            const path = this.basePath + '/pet/findByTags';

            let queryParameters: any = {};
            let headerParams: any = this.extendObj({}, this.defaultHeaders);





            if (tags !== undefined) {
                queryParameters['tags'] = tags;
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


        public getPetById (petId: number, extraHttpRequestParams?: any ) : ng.IHttpPromise<Pet> {
            const path = this.basePath + '/pet/{petId}'
                .replace('{' + 'petId' + '}', String(petId));

            let queryParameters: any = {};
            let headerParams: any = this.extendObj({}, this.defaultHeaders);



            // verify required parameter 'petId' is set
            if (!petId) {
                throw new Error('Missing required parameter petId when calling getPetById');
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


        public updatePetWithForm (petId: string, name?: string, status?: string, extraHttpRequestParams?: any ) : ng.IHttpPromise<{}> {
            const path = this.basePath + '/pet/{petId}'
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
                url: path,
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


        public deletePet (petId: number, apiKey?: string, extraHttpRequestParams?: any ) : ng.IHttpPromise<{}> {
            const path = this.basePath + '/pet/{petId}'
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


        public uploadFile (petId: number, additionalMetadata?: string, file?: any, extraHttpRequestParams?: any ) : ng.IHttpPromise<{}> {
            const path = this.basePath + '/pet/{petId}/uploadImage'
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
                url: path,
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

