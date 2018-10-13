"use strict";
var __extends = (this && this.__extends) || (function () {
    var extendStatics = function (d, b) {
        extendStatics = Object.setPrototypeOf ||
            ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
            function (d, b) { for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p]; };
        return extendStatics(d, b);
    }
    return function (d, b) {
        extendStatics(d, b);
        function __() { this.constructor = d; }
        d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
    };
})();
Object.defineProperty(exports, "__esModule", { value: true });
var baseapi_1 = require("./baseapi");
var http_1 = require("../http/http");
var ObjectSerializer_1 = require("../models/ObjectSerializer");
var UserApiRequestFactory = (function (_super) {
    __extends(UserApiRequestFactory, _super);
    function UserApiRequestFactory() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    UserApiRequestFactory.prototype.createUser = function (user, options) {
        if (user === null || user === undefined) {
            throw new baseapi_1.RequiredError('Required parameter user was null or undefined when calling createUser.');
        }
        var localVarPath = '/user';
        var requestContext = this.configuration.baseServer.makeRequestContext(localVarPath, http_1.HttpMethod.POST);
        requestContext.setHeaderParam("Content-Type", "application/json");
        var needsSerialization = ("User" !== "string") || requestContext.getHeaders()['Content-Type'] === 'application/json';
        var serializedBody = needsSerialization ? JSON.stringify(user || {}) : (user.toString() || "");
        requestContext.setBody(serializedBody);
        return requestContext;
    };
    UserApiRequestFactory.prototype.createUsersWithArrayInput = function (user, options) {
        if (user === null || user === undefined) {
            throw new baseapi_1.RequiredError('Required parameter user was null or undefined when calling createUsersWithArrayInput.');
        }
        var localVarPath = '/user/createWithArray';
        var requestContext = this.configuration.baseServer.makeRequestContext(localVarPath, http_1.HttpMethod.POST);
        requestContext.setHeaderParam("Content-Type", "application/json");
        var needsSerialization = ("Array&lt;User&gt;" !== "string") || requestContext.getHeaders()['Content-Type'] === 'application/json';
        var serializedBody = needsSerialization ? JSON.stringify(user || {}) : (user.toString() || "");
        requestContext.setBody(serializedBody);
        return requestContext;
    };
    UserApiRequestFactory.prototype.createUsersWithListInput = function (user, options) {
        if (user === null || user === undefined) {
            throw new baseapi_1.RequiredError('Required parameter user was null or undefined when calling createUsersWithListInput.');
        }
        var localVarPath = '/user/createWithList';
        var requestContext = this.configuration.baseServer.makeRequestContext(localVarPath, http_1.HttpMethod.POST);
        requestContext.setHeaderParam("Content-Type", "application/json");
        var needsSerialization = ("Array&lt;User&gt;" !== "string") || requestContext.getHeaders()['Content-Type'] === 'application/json';
        var serializedBody = needsSerialization ? JSON.stringify(user || {}) : (user.toString() || "");
        requestContext.setBody(serializedBody);
        return requestContext;
    };
    UserApiRequestFactory.prototype.deleteUser = function (username, options) {
        if (username === null || username === undefined) {
            throw new baseapi_1.RequiredError('Required parameter username was null or undefined when calling deleteUser.');
        }
        var localVarPath = '/user/{username}'
            .replace('{' + 'username' + '}', encodeURIComponent(String(username)));
        var requestContext = this.configuration.baseServer.makeRequestContext(localVarPath, http_1.HttpMethod.DELETE);
        return requestContext;
    };
    UserApiRequestFactory.prototype.getUserByName = function (username, options) {
        if (username === null || username === undefined) {
            throw new baseapi_1.RequiredError('Required parameter username was null or undefined when calling getUserByName.');
        }
        var localVarPath = '/user/{username}'
            .replace('{' + 'username' + '}', encodeURIComponent(String(username)));
        var requestContext = this.configuration.baseServer.makeRequestContext(localVarPath, http_1.HttpMethod.GET);
        return requestContext;
    };
    UserApiRequestFactory.prototype.loginUser = function (username, password, options) {
        if (username === null || username === undefined) {
            throw new baseapi_1.RequiredError('Required parameter username was null or undefined when calling loginUser.');
        }
        if (password === null || password === undefined) {
            throw new baseapi_1.RequiredError('Required parameter password was null or undefined when calling loginUser.');
        }
        var localVarPath = '/user/login';
        var requestContext = this.configuration.baseServer.makeRequestContext(localVarPath, http_1.HttpMethod.GET);
        if (username !== undefined) {
            requestContext.setQueryParam("", ObjectSerializer_1.ObjectSerializer.serialize(username, "string"));
        }
        if (password !== undefined) {
            requestContext.setQueryParam("", ObjectSerializer_1.ObjectSerializer.serialize(password, "string"));
        }
        return requestContext;
    };
    UserApiRequestFactory.prototype.logoutUser = function (options) {
        var localVarPath = '/user/logout';
        var requestContext = this.configuration.baseServer.makeRequestContext(localVarPath, http_1.HttpMethod.GET);
        return requestContext;
    };
    UserApiRequestFactory.prototype.updateUser = function (username, user, options) {
        if (username === null || username === undefined) {
            throw new baseapi_1.RequiredError('Required parameter username was null or undefined when calling updateUser.');
        }
        if (user === null || user === undefined) {
            throw new baseapi_1.RequiredError('Required parameter user was null or undefined when calling updateUser.');
        }
        var localVarPath = '/user/{username}'
            .replace('{' + 'username' + '}', encodeURIComponent(String(username)));
        var requestContext = this.configuration.baseServer.makeRequestContext(localVarPath, http_1.HttpMethod.PUT);
        requestContext.setHeaderParam("Content-Type", "application/json");
        var needsSerialization = ("User" !== "string") || requestContext.getHeaders()['Content-Type'] === 'application/json';
        var serializedBody = needsSerialization ? JSON.stringify(user || {}) : (user.toString() || "");
        requestContext.setBody(serializedBody);
        return requestContext;
    };
    return UserApiRequestFactory;
}(baseapi_1.BaseAPIRequestFactory));
exports.UserApiRequestFactory = UserApiRequestFactory;
var UserApiResponseProcessor = (function () {
    function UserApiResponseProcessor() {
    }
    UserApiResponseProcessor.prototype.createUser = function (response) {
        var responseOK = response.httpStatusCode && response.httpStatusCode >= 200 && response.httpStatusCode <= 299;
        if (!responseOK) {
            throw new Error("Invalid status code: " + response.httpStatusCode + "!");
        }
    };
    UserApiResponseProcessor.prototype.createUsersWithArrayInput = function (response) {
        var responseOK = response.httpStatusCode && response.httpStatusCode >= 200 && response.httpStatusCode <= 299;
        if (!responseOK) {
            throw new Error("Invalid status code: " + response.httpStatusCode + "!");
        }
    };
    UserApiResponseProcessor.prototype.createUsersWithListInput = function (response) {
        var responseOK = response.httpStatusCode && response.httpStatusCode >= 200 && response.httpStatusCode <= 299;
        if (!responseOK) {
            throw new Error("Invalid status code: " + response.httpStatusCode + "!");
        }
    };
    UserApiResponseProcessor.prototype.deleteUser = function (response) {
        var responseOK = response.httpStatusCode && response.httpStatusCode >= 200 && response.httpStatusCode <= 299;
        if (!responseOK) {
            throw new Error("Invalid status code: " + response.httpStatusCode + "!");
        }
    };
    UserApiResponseProcessor.prototype.getUserByName = function (response) {
        var responseOK = response.httpStatusCode && response.httpStatusCode >= 200 && response.httpStatusCode <= 299;
        var body = ObjectSerializer_1.ObjectSerializer.deserialize(response.body, "User");
        if (responseOK) {
            return body;
        }
        else {
            throw body;
        }
    };
    UserApiResponseProcessor.prototype.loginUser = function (response) {
        var responseOK = response.httpStatusCode && response.httpStatusCode >= 200 && response.httpStatusCode <= 299;
        var body = ObjectSerializer_1.ObjectSerializer.deserialize(response.body, "string");
        if (responseOK) {
            return body;
        }
        else {
            throw body;
        }
    };
    UserApiResponseProcessor.prototype.logoutUser = function (response) {
        var responseOK = response.httpStatusCode && response.httpStatusCode >= 200 && response.httpStatusCode <= 299;
        if (!responseOK) {
            throw new Error("Invalid status code: " + response.httpStatusCode + "!");
        }
    };
    UserApiResponseProcessor.prototype.updateUser = function (response) {
        var responseOK = response.httpStatusCode && response.httpStatusCode >= 200 && response.httpStatusCode <= 299;
        if (!responseOK) {
            throw new Error("Invalid status code: " + response.httpStatusCode + "!");
        }
    };
    return UserApiResponseProcessor;
}());
exports.UserApiResponseProcessor = UserApiResponseProcessor;
//# sourceMappingURL=UserApi.js.map