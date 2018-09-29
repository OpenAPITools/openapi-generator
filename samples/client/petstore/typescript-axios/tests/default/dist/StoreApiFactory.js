"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var chai_1 = require("chai");
var typescript_axios_petstore_1 = require("@swagger/typescript-axios-petstore");
var typescript_axios_petstore_2 = require("@swagger/typescript-axios-petstore");
var config;
before(function () {
    config = new typescript_axios_petstore_2.Configuration();
    config.accessToken = "foobar";
    config.apiKey = function (securityName) {
        // for multiple apiKey security
        if (securityName === "api_key") {
            return "foobar";
        }
        return;
    };
    config.username = "foo";
    config.password = "bar";
});
describe("StoreApiFactory", function () {
    function runSuite(description, requestOptions) {
        describe(description, function () {
            it("should get inventory", function () {
                return typescript_axios_petstore_1.StoreApiFactory(config)
                    .getInventory(requestOptions)
                    .then(function (result) {
                    chai_1.expect(Object.keys(result.data)).to.not.be.empty;
                });
            });
        });
    }
    runSuite("without custom request options");
    runSuite("with custom request options", {
        credentials: "include",
        mode: "cors"
    });
});
