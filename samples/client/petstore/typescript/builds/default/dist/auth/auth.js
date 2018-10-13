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
var btoa = require("btoa");
var SecurityAuthentication = (function () {
    function SecurityAuthentication(name) {
        this.name = name;
    }
    SecurityAuthentication.prototype.getName = function () {
        return this.name;
    };
    return SecurityAuthentication;
}());
exports.SecurityAuthentication = SecurityAuthentication;
var NoAuthentication = (function (_super) {
    __extends(NoAuthentication, _super);
    function NoAuthentication() {
        return _super.call(this, "_no_auth") || this;
    }
    NoAuthentication.prototype.applySecurityAuthentication = function (_context) {
    };
    return NoAuthentication;
}(SecurityAuthentication));
exports.NoAuthentication = NoAuthentication;
var APIKeyAuthentication = (function (_super) {
    __extends(APIKeyAuthentication, _super);
    function APIKeyAuthentication(authName, paramName, keyLocation, apiKey) {
        var _this = _super.call(this, authName) || this;
        _this.paramName = paramName;
        _this.keyLocation = keyLocation;
        _this.apiKey = apiKey;
        return _this;
    }
    APIKeyAuthentication.prototype.applySecurityAuthentication = function (context) {
        if (this.keyLocation === "header") {
            context.setHeaderParam(this.paramName, this.apiKey);
        }
        else if (this.keyLocation === "cookie") {
            context.addCookie(this.paramName, this.apiKey);
        }
        else if (this.keyLocation === "query") {
            context.setQueryParam(this.paramName, this.apiKey);
        }
    };
    return APIKeyAuthentication;
}(SecurityAuthentication));
exports.APIKeyAuthentication = APIKeyAuthentication;
var HttpBasicAuthentication = (function (_super) {
    __extends(HttpBasicAuthentication, _super);
    function HttpBasicAuthentication(authName, username, password) {
        var _this = _super.call(this, authName) || this;
        _this.username = username;
        _this.password = password;
        return _this;
    }
    HttpBasicAuthentication.prototype.applySecurityAuthentication = function (context) {
        var comb = this.username + ":" + this.password;
        context.setHeaderParam("Authentication", "Basic " + btoa(comb));
    };
    return HttpBasicAuthentication;
}(SecurityAuthentication));
exports.HttpBasicAuthentication = HttpBasicAuthentication;
var OAuth2Authentication = (function (_super) {
    __extends(OAuth2Authentication, _super);
    function OAuth2Authentication(authName) {
        return _super.call(this, authName) || this;
    }
    OAuth2Authentication.prototype.applySecurityAuthentication = function (context) {
    };
    return OAuth2Authentication;
}(SecurityAuthentication));
exports.OAuth2Authentication = OAuth2Authentication;
function configureAuthMethods(conf) {
    var authMethods = {};
    if (!conf) {
        return authMethods;
    }
    if (conf["api_key"]) {
        authMethods["api_key"] = new APIKeyAuthentication("api_key", "api_key", "header", conf["api_key"]);
    }
    if (conf["petstore_auth"]) {
        authMethods["petstore_auth"] = new OAuth2Authentication("petstore_auth");
    }
    return authMethods;
}
exports.configureAuthMethods = configureAuthMethods;
//# sourceMappingURL=auth.js.map