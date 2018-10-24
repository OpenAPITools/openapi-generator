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
describe("PetApiFactory", function () {
    function runSuite(description, requestOptions, customAxiosInstance) {
        describe(description, function () {
            var fixture = createTestFixture();
            it("should add and delete Pet", function () {
                return typescript_axios_petstore_1.PetApiFactory(config, undefined, customAxiosInstance)
                    .addPet(fixture, requestOptions)
                    .then(function () { });
            });
            it("should get Pet by ID", function () {
                return typescript_axios_petstore_1.PetApiFactory(config, undefined, customAxiosInstance)
                    .getPetById(fixture.id, requestOptions)
                    .then(function (result) {
                    return chai_1.expect(result.data).to.deep.equal(fixture);
                });
            });
            it("should update Pet by ID", function () {
                return typescript_axios_petstore_1.PetApiFactory(config, undefined, customAxiosInstance)
                    .getPetById(fixture.id, requestOptions)
                    .then(function (result) {
                    result.data.name = "newname";
                    return typescript_axios_petstore_1.PetApiFactory(config)
                        .updatePet(result.data, requestOptions)
                        .then(function () {
                        return typescript_axios_petstore_1.PetApiFactory(config)
                            .getPetById(fixture.id, requestOptions)
                            .then(function (result) {
                            return chai_1.expect(result.data.name).to.deep.equal("newname");
                        });
                    });
                });
            });
            it("should delete Pet", function () {
                return typescript_axios_petstore_1.PetApiFactory(config, undefined, customAxiosInstance).deletePet(fixture.id, requestOptions);
            });
            it("should not contain deleted Pet", function () {
                return typescript_axios_petstore_1.PetApiFactory(config, undefined, customAxiosInstance)
                    .getPetById(fixture.id, requestOptions)
                    .then(function (result) {
                    return chai_1.expect(result.data).to.not.exist;
                }, function (err) {
                    return chai_1.expect(err).to.exist;
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
function createTestFixture(ts) {
    if (ts === void 0) { ts = Date.now(); }
    var category = {
        id: ts,
        name: "category" + ts
    };
    var pet = {
        id: ts,
        name: "pet" + ts,
        category: category,
        photoUrls: ["http://foo.bar.com/1", "http://foo.bar.com/2"],
        status: typescript_axios_petstore_1.Pet.StatusEnum.Available,
        tags: []
    };
    return pet;
}
