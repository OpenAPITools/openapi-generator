import * as petstore from 'ts-petstore-client'

import { expect } from "chai";
import * as fs from 'fs';

const configuration = petstore.createConfiguration()
const petApi = new petstore.PetApi(configuration)

const tag = new petstore.Tag();
tag.name = "tag1"
tag.id = Math.floor(Math.random() * 100000)

let pet: petstore.Pet;

describe("PetApi", () => {
    beforeEach(async () => {
        pet = new petstore.Pet()
        pet.id = Math.floor(Math.random() * 100000)
        pet.name = "PetName"
        pet.photoUrls = []
        pet.status = 'available'
        pet.tags = [ tag ]
        pet.category = undefined

        await petApi.addPet(pet);
    });

    it("addPet", async () => {
        const createdPet = await petApi.getPetById(pet.id)
        expect(createdPet).to.deep.equal(pet);
    })

    it("deletePet", async () => {
        await petApi.deletePet(pet.id);
        let deletedPet;
        try {
            deletedPet = await petApi.getPetById(pet.id)
        } catch (err) {
            expect(err.code).to.equal(404);
            expect(err.message).to.include("Pet not found");
            return;
        }
        throw new Error("Pet with id " + deletedPet.id + " was not deleted!");
    })

    it("deleteNonExistantPet", async () => {
        const nonExistantId = Math.floor(Math.random() * 100000);
        try {
            await petApi.deletePet(nonExistantId)
        } catch (err) {
            // The 404 response for this endpoint is officially documented, but
            // that documentation is not used for generating the client code.
            // That means we get an error about the response being undefined
            // here.
            expect(err.code).to.equal(404);
            expect(err.message).to.include("Unknown API Status Code");
            return;
        }
        throw new Error("Deleted non-existant pet with id " + nonExistantId + "!");
    })

    it("findPetsByStatus", async () => {
        const pets = await petApi.findPetsByStatus(["available"]);
        expect(pets.length).to.be.at.least(1);
    })

    it("findPetsByTag", async () => {
        const pets = await petApi.findPetsByTags([tag.name])
        expect(pets.length).to.be.at.least(1);
    })

    it("getPetById", async () => {
        const returnedPet = await petApi.getPetById(pet.id);
        expect(returnedPet).to.deep.equal(pet);
    })

    it("updatePet", async () => {
        pet.name = "updated name";
        await petApi.updatePet(pet);
        await petApi.updatePet(pet);

        const returnedPet = await petApi.getPetById(pet.id);
        expect(returnedPet.id).to.equal(pet.id)
        expect(returnedPet.name).to.equal(pet.name);
    })

    it("updatePetWithForm", async () => {
        const updatedName = "updated name";
        await petApi.updatePetWithForm(pet.id, updatedName);

        const returnedPet = await petApi.getPetById(pet.id)
        expect(returnedPet.id).to.equal(pet.id)
        expect(returnedPet.name).to.equal(updatedName);
    })

    it("uploadFile", async () => {
        const image = fs.readFileSync(__dirname + "/pet.png")
        const response = await petApi.uploadFile(pet.id, "Metadata", { name: "pet.png", data: image});
        expect(response.code).to.be.gte(200).and.lt(300);
        expect(response.message).to.contain("pet.png");
    })
})