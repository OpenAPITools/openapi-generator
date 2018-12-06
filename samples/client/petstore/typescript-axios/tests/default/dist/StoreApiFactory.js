"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var chai_1 = require("chai");
var typescript_axios_petstore_1 = require("@swagger/typescript-axios-petstore");
var typescript_axios_petstore_2 = require("@swagger/typescript-axios-petstore");
var axios_1 = require("axios");
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
    function runSuite(description, requestOptions, customAxiosInstance) {
        describe(description, function () {
            it("should get inventory", function () {
                return typescript_axios_petstore_1.StoreApiFactory(config, undefined, customAxiosInstance)
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
    runSuite("without custom axios instance");
    runSuite("with custom axios instance", {}, axios_1.default);
    runSuite("with custom request options and custom axios instance", {
        credentials: "include",
        mode: "cors"
    }, axios_1.default);
});
