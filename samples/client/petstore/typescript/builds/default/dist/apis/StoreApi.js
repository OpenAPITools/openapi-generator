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
var StoreApiRequestFactory = (function (_super) {
    __extends(StoreApiRequestFactory, _super);
    function StoreApiRequestFactory() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    StoreApiRequestFactory.prototype.deleteOrder = function (orderId, options) {
        if (orderId === null || orderId === undefined) {
            throw new baseapi_1.RequiredError('Required parameter orderId was null or undefined when calling deleteOrder.');
        }
        var localVarPath = '/store/order/{orderId}'
            .replace('{' + 'orderId' + '}', encodeURIComponent(String(orderId)));
        var requestContext = this.configuration.baseServer.makeRequestContext(localVarPath, http_1.HttpMethod.DELETE);
        return requestContext;
    };
    StoreApiRequestFactory.prototype.getInventory = function (options) {
        var localVarPath = '/store/inventory';
        var requestContext = this.configuration.baseServer.makeRequestContext(localVarPath, http_1.HttpMethod.GET);
        var authMethod = null;
        authMethod = this.configuration.authMethods["api_key"];
        if (authMethod) {
            authMethod.applySecurityAuthentication(requestContext);
        }
        return requestContext;
    };
    StoreApiRequestFactory.prototype.getOrderById = function (orderId, options) {
        if (orderId === null || orderId === undefined) {
            throw new baseapi_1.RequiredError('Required parameter orderId was null or undefined when calling getOrderById.');
        }
        var localVarPath = '/store/order/{orderId}'
            .replace('{' + 'orderId' + '}', encodeURIComponent(String(orderId)));
        var requestContext = this.configuration.baseServer.makeRequestContext(localVarPath, http_1.HttpMethod.GET);
        return requestContext;
    };
    StoreApiRequestFactory.prototype.placeOrder = function (order, options) {
        if (order === null || order === undefined) {
            throw new baseapi_1.RequiredError('Required parameter order was null or undefined when calling placeOrder.');
        }
        var localVarPath = '/store/order';
        var requestContext = this.configuration.baseServer.makeRequestContext(localVarPath, http_1.HttpMethod.POST);
        requestContext.setHeaderParam("Content-Type", "application/json");
        var needsSerialization = ("Order" !== "string") || requestContext.getHeaders()['Content-Type'] === 'application/json';
        var serializedBody = needsSerialization ? JSON.stringify(order || {}) : (order.toString() || "");
        requestContext.setBody(serializedBody);
        return requestContext;
    };
    return StoreApiRequestFactory;
}(baseapi_1.BaseAPIRequestFactory));
exports.StoreApiRequestFactory = StoreApiRequestFactory;
var StoreApiResponseProcessor = (function () {
    function StoreApiResponseProcessor() {
    }
    StoreApiResponseProcessor.prototype.deleteOrder = function (response) {
        var responseOK = response.httpStatusCode && response.httpStatusCode >= 200 && response.httpStatusCode <= 299;
        if (!responseOK) {
            throw new Error("Invalid status code: " + response.httpStatusCode + "!");
        }
    };
    StoreApiResponseProcessor.prototype.getInventory = function (response) {
        var responseOK = response.httpStatusCode && response.httpStatusCode >= 200 && response.httpStatusCode <= 299;
        var body = ObjectSerializer_1.ObjectSerializer.deserialize(response.body, "{ [key: string]: number; }");
        if (responseOK) {
            return body;
        }
        else {
            throw body;
        }
    };
    StoreApiResponseProcessor.prototype.getOrderById = function (response) {
        var responseOK = response.httpStatusCode && response.httpStatusCode >= 200 && response.httpStatusCode <= 299;
        var body = ObjectSerializer_1.ObjectSerializer.deserialize(response.body, "Order");
        if (responseOK) {
            return body;
        }
        else {
            throw body;
        }
    };
    StoreApiResponseProcessor.prototype.placeOrder = function (response) {
        var responseOK = response.httpStatusCode && response.httpStatusCode >= 200 && response.httpStatusCode <= 299;
        var body = ObjectSerializer_1.ObjectSerializer.deserialize(response.body, "Order");
        if (responseOK) {
            return body;
        }
        else {
            throw body;
        }
    };
    return StoreApiResponseProcessor;
}());
exports.StoreApiResponseProcessor = StoreApiResponseProcessor;
//# sourceMappingURL=StoreApi.js.map