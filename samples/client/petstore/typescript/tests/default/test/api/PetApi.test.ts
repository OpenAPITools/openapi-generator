import {PetApi, Configuration, Pet, ApiException} from 'ts-petstore-client'
import { expect, assert } from "chai";

const configuration = new Configuration()
const petApi = new PetApi(configuration)

const pet = new Pet()
pet.id = Math.floor(Math.random() * 100000)
pet.name = "PetName"
pet.photoUrls = []
pet.status = Pet.StatusEnum.Available
pet.tags = []
pet.category = undefined

describe("PetApi", () =>{ 
    it("addPet", (done) => {


        petApi.addPet(pet).then(() => {
            return petApi.getPetById(pet.id)
        }).then((createdPet) => {
            expect(createdPet).to.deep.equal(pet);
            done()
        }).catch((err) => {
            done(err)
        })
    })

    it("deletePet", (done) => {
        petApi.addPet(pet).then(() => {
            return petApi.deletePet(pet.id)
        }).then(() => {
            return petApi.getPetById(pet.id)
        }).then((pet: Pet) => {
            done("Pet with id " + pet.id + " was not deleted!");
        }).catch((err: any) => {
            if (err instanceof ApiException && err.code == 404) {
                done();                
            } else {
                done(err)
            }
        })
    })
})