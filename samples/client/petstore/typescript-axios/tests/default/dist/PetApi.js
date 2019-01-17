"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var chai_1 = require("chai");
var typescript_axios_petstore_1 = require("@swagger/typescript-axios-petstore");
var axios_1 = require("axios");
describe("PetApi", function () {
    function runSuite(description, requestOptions, customAxiosInstance) {
        describe(description, function () {
            var api;
            var fixture = createTestFixture();
            beforeEach(function () {
                api = new typescript_axios_petstore_1.PetApi(undefined, undefined, customAxiosInstance);
            });
            it("should add and delete Pet", function () {
                return api.addPet(fixture, requestOptions).then(function () { });
            });
            it("should get Pet by ID", function () {
                return api
                    .getPetById(fixture.id, requestOptions)
                    .then(function (result) {
                    return chai_1.expect(result.data).to.deep.equal(fixture);
                });
            });
            it("should update Pet by ID", function () {
                return api
                    .getPetById(fixture.id, requestOptions)
                    .then(function (response) {
                    var result = response.data;
                    result.name = "newname";
                    return api.updatePet(result, requestOptions).then(function () {
                        return api
                            .getPetById(fixture.id, requestOptions)
                            .then(function (response) {
                            return chai_1.expect(response.data.name).to.deep.equal("newname");
                        });
                    });
                });
            });
            it("should delete Pet", function () {
                return api.deletePet(fixture.id, requestOptions);
            });
            it("should not contain deleted Pet", function () {
                return api.getPetById(fixture.id, requestOptions).then(function (result) {
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
