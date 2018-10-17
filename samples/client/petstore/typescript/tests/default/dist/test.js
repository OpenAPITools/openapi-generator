"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var ts_petstore_client_1 = require("ts-petstore-client");
var MiddlewareA = /** @class */ (function () {
    function MiddlewareA() {
    }
    MiddlewareA.prototype.pre = function (request) {
        console.log(request);
        return Promise.resolve(request);
    };
    MiddlewareA.prototype.post = function (response) {
        console.log(response);
        return Promise.resolve(response);
    };
    return MiddlewareA;
}());
var config = new ts_petstore_client_1.Configuration({
    middleware: [
        new MiddlewareA()
    ]
});
var api = new ts_petstore_client_1.PetApi(config);
api.getPetById(3).then(function (pet) {
    console.log(pet);
}).catch(function (err) {
    console.log(err);
});
