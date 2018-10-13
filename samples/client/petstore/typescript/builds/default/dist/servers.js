"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var http_1 = require("./http/http");
var ServerConfiguration = (function () {
    function ServerConfiguration(url) {
        this.url = url;
    }
    ServerConfiguration.prototype.makeRequestContext = function (endpoint, httpMethod) {
        return new http_1.RequestContext(this.url + endpoint, httpMethod);
    };
    return ServerConfiguration;
}());
exports.ServerConfiguration = ServerConfiguration;
exports.servers = [
    new ServerConfiguration("http://petstore.swagger.io/v2"),
];
//# sourceMappingURL=servers.js.map