declare var QUnit: any;

import * as petstore from 'ts-petstore-client'

const configuration = new petstore.Configuration()
const petApi = new petstore.apis.PromisePetApi(configuration)

const tag = new petstore.models.Tag();
tag.name = "tag1"
tag.id = Math.floor(Math.random() * 100000)

const pet = new petstore.models.Pet()
pet.id = Math.floor(Math.random() * 100000)
pet.name = "PetName"
pet.photoUrls = []
pet.status = 'available'
pet.tags = [ tag ]
pet.category = undefined

QUnit.module("PetApi")

QUnit.test("addPet", (assert: any) => {
    return petApi.addPet(pet).then(() => {
        return petApi.getPetById(pet.id)
    }).then((createdPet: petstore.models.Pet) => {
        assert.deepEqual(createdPet, pet);
    })
})

QUnit.test("deletePet", (assert: any) => {
    return petApi.addPet(pet).then(() => {
        return petApi.deletePet(pet.id)
    }).then(() => {
        return petApi.getPetById(pet.id)
    }).then((pet: petstore.models.Pet) => {
        throw new Error("Pet with id " + pet.id + " was not deleted!");
    }).catch((err: any) => {
        // pet does not exist
        if (err.code && err.code == 404) {
            assert.ok(true, "404'd on getPet after deletion - all good.")
            return;             
        } else {
            throw err;
        }
    })
})

QUnit.test("findPetsByStatus", (assert: any) => {
    return petApi.addPet(pet).then(() => {
        return petApi.findPetsByStatus(["available"])
    }).then((pets: petstore.models.Pet[]) => {
        assert.ok(pets.length >= 1, "Found at least one pet.");
    })
})

// bugged on server side! Code 500
/*    QUnit.test("findPetsByTag", (done) => {
    petApi.addPet(pet).then(() => {
        return petApi.findPetsByTags([tag.name])
    }).then((pets: Pet[]) => {
        expect(pets.length).to.be.at.least(1);
        done();
    }).catch((err) => {
        done(err);
    })
})*/

QUnit.test("getPetById", (assert: any) => {
    return petApi.addPet(pet).then(() => {
        return petApi.getPetById(pet.id)
    }).then((returnedPet: petstore.models.Pet) => {
        assert.deepEqual(returnedPet, pet);
    })
})

QUnit.test("updatePet", (assert: any) => {
    const oldName = pet.name
    const updatedName = "updated name";
    return petApi.addPet(pet).then(() => {
        pet.name = updatedName
        return petApi.updatePet(pet).then(() => {
            pet.name = oldName;
        }).catch((err) => {
            pet.name = oldName
            throw err;
        });
    }).then(() => {
        return petApi.getPetById(pet.id);
    }).then((returnedPet: petstore.models.Pet) => {
        assert.equal(returnedPet.id, pet.id)
        assert.equal(returnedPet.name, updatedName);
    })
})

// not supported by online swagger api?
/*    QUnit.test("updatePetWithForm", (done) => {
    const updatedName = "updated name";
    petApi.addPet(pet).then(() => {
        return petApi.updatePetWithForm(pet.id, updatedName)
    }).then(() => {
        return petApi.getPetById(pet.id)
    }).then((returnedPet: Pet) => {
        expect(returnedPet.id).to.equal(pet.id)
        expect(returnedPet.name).to.equal(updatedName);
        done()
    }).catch((err) => {
        done(err)
    })
})*/

/** not easily possible?
QUnit.test("uploadFile", (done) => {
    const image = fs.readFileSync(__dirname + "/pet.png")
    petApi.uploadFile(pet.id, "Metadata", { name: "pet.png", data: image}).then((response: any) => {
        expect(response.code).to.be.gte(200).and.lt(300);
        expect(response.message).to.contain("pet.png");
        done();
    }).catch((err) => {
        done(err);
    })
})*/
