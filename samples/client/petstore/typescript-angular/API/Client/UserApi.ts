/// <reference path="api.d.ts" />

/* tslint:disable:no-unused-variable member-ordering */

module API.Client {
    'use strict';

    export class UserApi {
        private basePath = 'http://petstore.swagger.io/v2';

        static $inject: string[] = ['$http', '$httpParamSerializer'];

        constructor(private $http: ng.IHttpService, basePath?: string, private $httpParamSerializer?: (any) => any) {
            if (basePath) {
                this.basePath = basePath;
            }
        }

        public createUser (body?: User, extraHttpRequestParams?: any ) : ng.IHttpPromise<{}> {
            var path = this.basePath + '/user';

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

        public createUsersWithArrayInput (body?: Array<User>, extraHttpRequestParams?: any ) : ng.IHttpPromise<{}> {
            var path = this.basePath + '/user/createWithArray';

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

        public createUsersWithListInput (body?: Array<User>, extraHttpRequestParams?: any ) : ng.IHttpPromise<{}> {
            var path = this.basePath + '/user/createWithList';

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

        public loginUser (username?: string, password?: string, extraHttpRequestParams?: any ) : ng.IHttpPromise<string> {
            var path = this.basePath + '/user/login';

            var queryParameters: any = {};
            var headerParams: any = {};
            if (username !== undefined) {
                queryParameters['username'] = username;
            }

            if (password !== undefined) {
                queryParameters['password'] = password;
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

        public logoutUser (extraHttpRequestParams?: any ) : ng.IHttpPromise<{}> {
            var path = this.basePath + '/user/logout';

            var queryParameters: any = {};
            var headerParams: any = {};
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

        public getUserByName (username: string, extraHttpRequestParams?: any ) : ng.IHttpPromise<User> {
            var path = this.basePath + '/user/{username}';

            path = path.replace('{' + 'username' + '}', String(username));

            var queryParameters: any = {};
            var headerParams: any = {};
            // verify required parameter 'username' is set
            if (!username) {
                throw new Error('Missing required parameter username when calling getUserByName');
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

        public updateUser (username: string, body?: User, extraHttpRequestParams?: any ) : ng.IHttpPromise<{}> {
            var path = this.basePath + '/user/{username}';

            path = path.replace('{' + 'username' + '}', String(username));

            var queryParameters: any = {};
            var headerParams: any = {};
            // verify required parameter 'username' is set
            if (!username) {
                throw new Error('Missing required parameter username when calling updateUser');
            }

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

        public deleteUser (username: string, extraHttpRequestParams?: any ) : ng.IHttpPromise<{}> {
            var path = this.basePath + '/user/{username}';

            path = path.replace('{' + 'username' + '}', String(username));

            var queryParameters: any = {};
            var headerParams: any = {};
            // verify required parameter 'username' is set
            if (!username) {
                throw new Error('Missing required parameter username when calling deleteUser');
            }

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
    }
}
