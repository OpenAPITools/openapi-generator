import { expect } from '@esm-bundle/chai';
import { ServerConfiguration, createConfiguration, PetApi, Tag, Pet, PetStatusEnum, ApiException, RequiredError } from 'ts-petstore-client'
import image from "./pet";

const configuration = createConfiguration({
    baseServer: new ServerConfiguration("http://localhost/v2", {}),
})
const petApi = new PetApi(configuration)

function createTag() {
    const tag = new Tag();
    tag.name = "tag1"
    tag.id = Math.floor(Math.random() * 100000)
    return tag as Required<Tag>;
}
const tag = createTag();

function createPet() {
    const pet = new Pet()
    pet.id = Math.floor(Math.random() * 100000)
    pet.name = "PetName"
    pet.photoUrls = []
    pet.status = PetStatusEnum.Available
    pet.tags = [ tag ]
    return pet as Required<Pet>;
}
let pet: Required<Pet>;

describe("PetApi", () => {
    beforeEach(async () => {
        pet = createPet();
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
        } catch (error) {
            const err = error as ApiException<unknown>;
            expect(err.code).to.equal(404);
            expect(err.message).to.include("Pet not found");
            return;
        }
        throw new Error("Pet with id " + deletedPet.id + " was not deleted!");
    })

    it("deleteNonExistentPet", async () => {
        // Use an id that is too big for the server to handle.
        const nonExistentId = 100000000000000000000000000.0;
        try {
            await petApi.deletePet(nonExistentId)
        } catch (error) {
            const err = error as ApiException<unknown>;
            // The 404 response for this endpoint is officially documented, but
            // that documentation is not used for generating the client code.
            // That means we get an error about the response being undefined
            // here.
            expect(err.code).to.equal(404);
            expect(err.message).to.include("Unknown API Status Code");
            expect(err.body).to.include("404");
            expect(err.body).to.include("message");
            return;
        }
        throw new Error("Deleted non-existent pet with id " + nonExistentId + "!");
    })

    it("failRunTimeRequiredParameterCheck", async () => {
        try {
            await petApi.deletePet(null as unknown as number)
        } catch (error) {
            const err = error as RequiredError;
            expect(err.api).to.equal("PetApi");
            expect(err.message).to.include("PetApi");
            expect(err.method).to.equal("deletePet");
            expect(err.message).to.include("deletePet");
            expect(err.field).to.equal("petId");
            expect(err.message).to.include("petId");
            return;
        }
        throw new Error("Accepted missing parameter!");
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
        const imageResponse = await fetch(image);
        const imageFile = new File([await imageResponse.blob()], "pet.png", { type: "image/png" });
        const response = await petApi.uploadFile(pet.id, "Metadata", imageFile);
        expect(response.code).to.be.gte(200).and.lt(300);
        expect(response.message).to.contain("pet.png");
    })
})
