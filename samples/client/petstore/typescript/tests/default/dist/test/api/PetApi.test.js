"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var ts_petstore_client_1 = require("ts-petstore-client");
var chai_1 = require("chai");
var configuration = new ts_petstore_client_1.Configuration();
var petApi = new ts_petstore_client_1.PetApi(configuration);
describe("PetApi", function () {
    it("addPet", function (done) {
        var pet = new ts_petstore_client_1.Pet();
        pet.id = Math.floor(Math.random() * 100000);
        pet.name = "PetName";
        pet.photoUrls = [];
        pet.status = ts_petstore_client_1.Pet.StatusEnum.Available;
        pet.tags = [];
        pet.category = undefined;
        petApi.addPet(pet).then(function () {
            return petApi.getPetById(pet.id);
        }).then(function (createdPet) {
            chai_1.expect(createdPet).to.deep.equal(pet);
            done();
        }).catch(function (err) {
            done(err);
        });
    });
});
