"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var chai_1 = require("chai");
var typescript_axios_petstore_1 = require("@swagger/typescript-axios-petstore");
var axios_1 = require("axios");
describe("StoreApi", function () {
    function runSuite(description, requestOptions, customAxiosInstance) {
        describe(description, function () {
            var api;
            beforeEach(function () {
                api = new typescript_axios_petstore_1.StoreApi(undefined, undefined, customAxiosInstance);
            });
            it("should get inventory", function () {
                return api
                    .getInventory(requestOptions)
                    .then(function (result) {
                    chai_1.expect(Object.keys(result)).to.not.be.empty;
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
