"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var isomorphic_fetch_1 = require("./http/isomorphic-fetch");
var servers_1 = require("./servers");
var auth_1 = require("./auth/auth");
var Configuration = (function () {
    function Configuration(conf) {
        if (conf === void 0) { conf = {}; }
        this.baseServer = conf.baseServer !== undefined ? conf.baseServer : servers_1.servers[0];
        this.httpApi = conf.httpApi || new isomorphic_fetch_1.IsomorphicFetchHttpLibrary();
        this.middleware = conf.middleware || [];
        this.authMethods = auth_1.configureAuthMethods(conf.authMethods);
    }
    return Configuration;
}());
exports.Configuration = Configuration;
//# sourceMappingURL=configuration.js.map