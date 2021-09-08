import * as petstore from 'ts-petstore-client'

import { expect } from "chai";
import * as fs from 'fs';

const configuration = petstore.createConfiguration()
const petApi = new petstore.PetApi(configuration)

const tag = new petstore.Tag();
tag.name = "tag1"
tag.id = Math.floor(Math.random() * 100000)

let pet: petstore.Pet;

// NOTE: There seem to be two instances of the api server
// As an ugly workaround we do all modifications two times

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
        await petApi.addPet(pet);
    });

    it("addPet", async () => {
        const createdPet = await petApi.getPetById(pet.id)
        expect(createdPet).to.deep.equal(pet);
    })

    it("deletePet", async () => {
        await petApi.deletePet(pet.id)
        await petApi.deletePet(pet.id)
        let deletedPet;
        try {
            deletedPet = await petApi.getPetById(pet.id)
        } catch (err) {
            expect(err.code).to.equal(404);
            return;
        }
        throw new Error("Pet with id " + deletedPet.id + " was not deleted!");
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