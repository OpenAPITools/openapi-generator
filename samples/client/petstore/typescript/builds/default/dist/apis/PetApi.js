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
var FormData = require("form-data");
var ObjectSerializer_1 = require("../models/ObjectSerializer");
var PetApiRequestFactory = (function (_super) {
    __extends(PetApiRequestFactory, _super);
    function PetApiRequestFactory() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    PetApiRequestFactory.prototype.addPet = function (pet, options) {
        if (pet === null || pet === undefined) {
            throw new baseapi_1.RequiredError('Required parameter pet was null or undefined when calling addPet.');
        }
        var localVarPath = '/pet';
        var requestContext = this.configuration.baseServer.makeRequestContext(localVarPath, http_1.HttpMethod.POST);
        requestContext.setHeaderParam("Content-Type", "application/json");
        var needsSerialization = ("Pet" !== "string") || requestContext.getHeaders()['Content-Type'] === 'application/json';
        var serializedBody = needsSerialization ? JSON.stringify(pet || {}) : (pet.toString() || "");
        requestContext.setBody(serializedBody);
        var authMethod = null;
        authMethod = this.configuration.authMethods["petstore_auth"];
        if (authMethod) {
            authMethod.applySecurityAuthentication(requestContext);
        }
        return requestContext;
    };
    PetApiRequestFactory.prototype.deletePet = function (petId, apiKey, options) {
        if (petId === null || petId === undefined) {
            throw new baseapi_1.RequiredError('Required parameter petId was null or undefined when calling deletePet.');
        }
        var localVarPath = '/pet/{petId}'
            .replace('{' + 'petId' + '}', encodeURIComponent(String(petId)));
        var requestContext = this.configuration.baseServer.makeRequestContext(localVarPath, http_1.HttpMethod.DELETE);
        requestContext.setHeaderParam("", ObjectSerializer_1.ObjectSerializer.serialize(apiKey, "string"));
        var authMethod = null;
        authMethod = this.configuration.authMethods["petstore_auth"];
        if (authMethod) {
            authMethod.applySecurityAuthentication(requestContext);
        }
        return requestContext;
    };
    PetApiRequestFactory.prototype.findPetsByStatus = function (status, options) {
        if (status === null || status === undefined) {
            throw new baseapi_1.RequiredError('Required parameter status was null or undefined when calling findPetsByStatus.');
        }
        var localVarPath = '/pet/findByStatus';
        var requestContext = this.configuration.baseServer.makeRequestContext(localVarPath, http_1.HttpMethod.GET);
        if (status !== undefined) {
            requestContext.setQueryParam("", ObjectSerializer_1.ObjectSerializer.serialize(status, "Array<'available' | 'pending' | 'sold'>"));
        }
        var authMethod = null;
        authMethod = this.configuration.authMethods["petstore_auth"];
        if (authMethod) {
            authMethod.applySecurityAuthentication(requestContext);
        }
        return requestContext;
    };
    PetApiRequestFactory.prototype.findPetsByTags = function (tags, options) {
        if (tags === null || tags === undefined) {
            throw new baseapi_1.RequiredError('Required parameter tags was null or undefined when calling findPetsByTags.');
        }
        var localVarPath = '/pet/findByTags';
        var requestContext = this.configuration.baseServer.makeRequestContext(localVarPath, http_1.HttpMethod.GET);
        if (tags !== undefined) {
            requestContext.setQueryParam("", ObjectSerializer_1.ObjectSerializer.serialize(tags, "Array<string>"));
        }
        var authMethod = null;
        authMethod = this.configuration.authMethods["petstore_auth"];
        if (authMethod) {
            authMethod.applySecurityAuthentication(requestContext);
        }
        return requestContext;
    };
    PetApiRequestFactory.prototype.getPetById = function (petId, options) {
        if (petId === null || petId === undefined) {
            throw new baseapi_1.RequiredError('Required parameter petId was null or undefined when calling getPetById.');
        }
        var localVarPath = '/pet/{petId}'
            .replace('{' + 'petId' + '}', encodeURIComponent(String(petId)));
        var requestContext = this.configuration.baseServer.makeRequestContext(localVarPath, http_1.HttpMethod.GET);
        var authMethod = null;
        authMethod = this.configuration.authMethods["api_key"];
        if (authMethod) {
            authMethod.applySecurityAuthentication(requestContext);
        }
        return requestContext;
    };
    PetApiRequestFactory.prototype.updatePet = function (pet, options) {
        if (pet === null || pet === undefined) {
            throw new baseapi_1.RequiredError('Required parameter pet was null or undefined when calling updatePet.');
        }
        var localVarPath = '/pet';
        var requestContext = this.configuration.baseServer.makeRequestContext(localVarPath, http_1.HttpMethod.PUT);
        requestContext.setHeaderParam("Content-Type", "application/json");
        var needsSerialization = ("Pet" !== "string") || requestContext.getHeaders()['Content-Type'] === 'application/json';
        var serializedBody = needsSerialization ? JSON.stringify(pet || {}) : (pet.toString() || "");
        requestContext.setBody(serializedBody);
        var authMethod = null;
        authMethod = this.configuration.authMethods["petstore_auth"];
        if (authMethod) {
            authMethod.applySecurityAuthentication(requestContext);
        }
        return requestContext;
    };
    PetApiRequestFactory.prototype.updatePetWithForm = function (petId, name, status, options) {
        if (petId === null || petId === undefined) {
            throw new baseapi_1.RequiredError('Required parameter petId was null or undefined when calling updatePetWithForm.');
        }
        var localVarPath = '/pet/{petId}'
            .replace('{' + 'petId' + '}', encodeURIComponent(String(petId)));
        var requestContext = this.configuration.baseServer.makeRequestContext(localVarPath, http_1.HttpMethod.POST);
        var localVarFormParams = new FormData();
        if (name !== undefined) {
            localVarFormParams.append('name', name);
        }
        if (status !== undefined) {
            localVarFormParams.append('status', status);
        }
        requestContext.setBody(localVarFormParams);
        var authMethod = null;
        authMethod = this.configuration.authMethods["petstore_auth"];
        if (authMethod) {
            authMethod.applySecurityAuthentication(requestContext);
        }
        return requestContext;
    };
    PetApiRequestFactory.prototype.uploadFile = function (petId, additionalMetadata, file, options) {
        if (petId === null || petId === undefined) {
            throw new baseapi_1.RequiredError('Required parameter petId was null or undefined when calling uploadFile.');
        }
        var localVarPath = '/pet/{petId}/uploadImage'
            .replace('{' + 'petId' + '}', encodeURIComponent(String(petId)));
        var requestContext = this.configuration.baseServer.makeRequestContext(localVarPath, http_1.HttpMethod.POST);
        var localVarFormParams = new FormData();
        if (additionalMetadata !== undefined) {
            localVarFormParams.append('additionalMetadata', additionalMetadata);
        }
        if (file !== undefined) {
            localVarFormParams.append('file', file);
        }
        requestContext.setBody(localVarFormParams);
        var authMethod = null;
        authMethod = this.configuration.authMethods["petstore_auth"];
        if (authMethod) {
            authMethod.applySecurityAuthentication(requestContext);
        }
        return requestContext;
    };
    return PetApiRequestFactory;
}(baseapi_1.BaseAPIRequestFactory));
exports.PetApiRequestFactory = PetApiRequestFactory;
var PetApiResponseProcessor = (function () {
    function PetApiResponseProcessor() {
    }
    PetApiResponseProcessor.prototype.addPet = function (response) {
        var responseOK = response.httpStatusCode && response.httpStatusCode >= 200 && response.httpStatusCode <= 299;
        if (!responseOK) {
            throw new Error("Invalid status code: " + response.httpStatusCode + "!");
        }
    };
    PetApiResponseProcessor.prototype.deletePet = function (response) {
        var responseOK = response.httpStatusCode && response.httpStatusCode >= 200 && response.httpStatusCode <= 299;
        if (!responseOK) {
            throw new Error("Invalid status code: " + response.httpStatusCode + "!");
        }
    };
    PetApiResponseProcessor.prototype.findPetsByStatus = function (response) {
        var responseOK = response.httpStatusCode && response.httpStatusCode >= 200 && response.httpStatusCode <= 299;
        var body = ObjectSerializer_1.ObjectSerializer.deserialize(response.body, "Array<Pet>");
        if (responseOK) {
            return body;
        }
        else {
            throw body;
        }
    };
    PetApiResponseProcessor.prototype.findPetsByTags = function (response) {
        var responseOK = response.httpStatusCode && response.httpStatusCode >= 200 && response.httpStatusCode <= 299;
        var body = ObjectSerializer_1.ObjectSerializer.deserialize(response.body, "Array<Pet>");
        if (responseOK) {
            return body;
        }
        else {
            throw body;
        }
    };
    PetApiResponseProcessor.prototype.getPetById = function (response) {
        var responseOK = response.httpStatusCode && response.httpStatusCode >= 200 && response.httpStatusCode <= 299;
        var body = ObjectSerializer_1.ObjectSerializer.deserialize(response.body, "Pet");
        if (responseOK) {
            return body;
        }
        else {
            throw body;
        }
    };
    PetApiResponseProcessor.prototype.updatePet = function (response) {
        var responseOK = response.httpStatusCode && response.httpStatusCode >= 200 && response.httpStatusCode <= 299;
        if (!responseOK) {
            throw new Error("Invalid status code: " + response.httpStatusCode + "!");
        }
    };
    PetApiResponseProcessor.prototype.updatePetWithForm = function (response) {
        var responseOK = response.httpStatusCode && response.httpStatusCode >= 200 && response.httpStatusCode <= 299;
        if (!responseOK) {
            throw new Error("Invalid status code: " + response.httpStatusCode + "!");
        }
    };
    PetApiResponseProcessor.prototype.uploadFile = function (response) {
        var responseOK = response.httpStatusCode && response.httpStatusCode >= 200 && response.httpStatusCode <= 299;
        var body = ObjectSerializer_1.ObjectSerializer.deserialize(response.body, "ApiResponse");
        if (responseOK) {
            return body;
        }
        else {
            throw body;
        }
    };
    return PetApiResponseProcessor;
}());
exports.PetApiResponseProcessor = PetApiResponseProcessor;
//# sourceMappingURL=PetApi.js.map