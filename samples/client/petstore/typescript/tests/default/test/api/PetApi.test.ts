import {PetApi, Configuration, Pet} from 'ts-petstore-client'
import { expect, assert } from "chai";

const configuration = new Configuration()
const petApi = new PetApi(configuration)

describe("PetApi", () =>{ 
    it("addPet", (done) => {
        const pet = new Pet()
        pet.id = Math.floor(Math.random() * 100000)
        pet.name = "PetName"
        pet.photoUrls = []
        pet.status = Pet.StatusEnum.Available
        pet.tags = []
        pet.category = undefined

        petApi.addPet(pet).then(() => {
            return petApi.getPetById(pet.id)
        }).then((createdPet) => {
            expect(createdPet).to.deep.equal(pet);
            done()
        }).catch((err) => {
            done(err)
        })
    })
})