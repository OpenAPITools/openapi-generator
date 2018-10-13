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
var URLParse = require("url-parse");
var HttpMethod;
(function (HttpMethod) {
    HttpMethod["GET"] = "GET";
    HttpMethod["HEAD"] = "HEAD";
    HttpMethod["POST"] = "POST";
    HttpMethod["PUT"] = "PUT";
    HttpMethod["DELETE"] = "DELETE";
    HttpMethod["CONNECT"] = "CONNECT";
    HttpMethod["OPTIONS"] = "OPTIONS";
    HttpMethod["TRACE"] = "TRACE";
    HttpMethod["PATCH"] = "PATCH";
})(HttpMethod = exports.HttpMethod || (exports.HttpMethod = {}));
var HttpException = (function (_super) {
    __extends(HttpException, _super);
    function HttpException(msg) {
        return _super.call(this, msg) || this;
    }
    return HttpException;
}(Error));
exports.HttpException = HttpException;
var RequestContext = (function () {
    function RequestContext(url, httpMethod) {
        this.httpMethod = httpMethod;
        this.headers = {};
        this.body = "";
        this.url = URLParse(url, true);
    }
    RequestContext.prototype.getUrl = function () {
        return this.url.toString();
    };
    RequestContext.prototype.setUrl = function (url) {
        this.url = URLParse(url, true);
    };
    RequestContext.prototype.setBody = function (body) {
        if (this.httpMethod === HttpMethod.GET) {
            throw new HttpException("Body should not be included in GET-Requests!");
        }
        this.body = body;
    };
    RequestContext.prototype.getHttpMethod = function () {
        return this.httpMethod;
    };
    RequestContext.prototype.getHeaders = function () {
        return this.headers;
    };
    RequestContext.prototype.getBody = function () {
        return this.body;
    };
    RequestContext.prototype.setQueryParam = function (name, value) {
        var queryObj = this.url.query;
        queryObj[name] = value;
        this.url.set("query", queryObj);
    };
    RequestContext.prototype.addCookie = function (name, value) {
        if (!this.headers["Cookie"]) {
            this.headers["Cookie"] = "";
        }
        this.headers["Cookie"] += name + "=" + value + "; ";
    };
    RequestContext.prototype.setHeaderParam = function (key, value) {
        this.headers[key] = value;
    };
    return RequestContext;
}());
exports.RequestContext = RequestContext;
var ResponseContext = (function () {
    function ResponseContext(httpStatusCode, headers, body) {
        this.httpStatusCode = httpStatusCode;
        this.headers = headers;
        this.body = body;
    }
    return ResponseContext;
}());
exports.ResponseContext = ResponseContext;
//# sourceMappingURL=http.js.map