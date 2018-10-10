"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var chai_1 = require("chai");
var typescript_axios_petstore_1 = require("@swagger/typescript-axios-petstore");
describe("StoreApi", function () {
    function runSuite(description, requestOptions) {
        describe(description, function () {
            var api;
            beforeEach(function () {
                api = new typescript_axios_petstore_1.StoreApi();
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
});
