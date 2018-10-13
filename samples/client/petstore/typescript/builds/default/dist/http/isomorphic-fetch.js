"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var http_1 = require("./http");
var e6p = require("es6-promise");
e6p.polyfill();
require("isomorphic-fetch");
var IsomorphicFetchHttpLibrary = (function () {
    function IsomorphicFetchHttpLibrary() {
    }
    IsomorphicFetchHttpLibrary.prototype.send = function (request) {
        var method = request.getHttpMethod().toString();
        var body = request.getBody();
        return fetch(request.getUrl(), {
            method: method,
            body: body,
            headers: request.getHeaders(),
            credentials: "same-origin"
        }).then(function (resp) {
            var headers = resp.headers._headers;
            for (var key in headers) {
                headers[key] = headers[key].join("; ");
            }
            return resp.text().then(function (body) {
                return new http_1.ResponseContext(resp.status, headers, body);
            });
        });
    };
    return IsomorphicFetchHttpLibrary;
}());
exports.IsomorphicFetchHttpLibrary = IsomorphicFetchHttpLibrary;
//# sourceMappingURL=isomorphic-fetch.js.map