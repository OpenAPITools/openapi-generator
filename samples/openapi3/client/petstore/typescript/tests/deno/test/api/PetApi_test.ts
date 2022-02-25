import {
  assertEquals,
  assert,
  fail,
} from "https://deno.land/std@0.82.0/testing/asserts.ts";
import * as petstore from "../../../../builds/deno/index.ts";

const configuration = petstore.createConfiguration();
const petApi = new petstore.PetApi(configuration);

const tag = new petstore.Tag();
tag.name = "tag1";
tag.id = Math.floor(Math.random() * 100000);

const pet = new petstore.Pet();
const petId = Math.floor(Math.random() * 100000);
pet.id = petId;
pet.name = "PetName";
pet.photoUrls = [];
pet.status = "available";
pet.tags = [tag];
pet.category = undefined;

Deno.test({
  name: "PetApi addPet getPetById",
  fn: async () => {
    await petApi.addPet(pet);
    const createdPet = await petApi.getPetById(petId);
    assertEquals(createdPet, pet);
  },
});

Deno.test({
  name: "PetApi deletePet",
  fn: async () => {
    try {
      await petApi.addPet(pet);
      await petApi.deletePet(petId);
      await petApi.getPetById(petId);
      fail("Pet with id " + pet.id + " was not deleted!");
    } catch (err) {
      if (!err.code || err.code != 404) {
        fail(err);
      }
    }
  },
  sanitizeResources: false,
});

Deno.test({
  name: "PetApi findPetsByStatus",
  fn: async () => {
    try {
      await petApi.addPet(pet);
      const pets = await petApi.findPetsByStatus(["available"]);
      assert(pets.length > 0);
    } catch (err) {
      fail(err);
    }
  },
});

// bugged on server side! Code 500
/*    it("findPetsByTag", (done) => {
        petApi.addPet(pet).then(() => {
            return petApi.findPetsByTags([tag.name])
        }).then((pets: Pet[]) => {
            expect(pets.length).to.be.at.least(1);
            done();
        }).catch((err: any) => {
            done(err);
        })
    })*/

Deno.test({
  name: "PetApi updatePet",
  fn: async () => {
    const oldName = pet.name;
    const updatedName = "updated name";
    try {
      await petApi.addPet(pet);
      pet.name = updatedName;
      await petApi.updatePet(pet);
      const updatedPet = await petApi.getPetById(petId);
      assertEquals(updatedPet.id, petId);
      assertEquals(updatedPet.name, updatedName);
    } catch (err) {
      fail(err);
    } finally {
      pet.name = oldName;
    }
  },
});

// not supported by online swagger api?
/*    it("updatePetWithForm", (done) => {
        const updatedName = "updated name";
        petApi.addPet(pet).then(() => {
            return petApi.updatePetWithForm(pet.id, updatedName)
        }).then(() => {
            return petApi.getPetById(pet.id)
        }).then((returnedPet: Pet) => {
            expect(returnedPet.id).to.equal(pet.id)
            expect(returnedPet.name).to.equal(updatedName);
            done()
        }).catch((err: any) => {
            done(err)
        })
    })*/

Deno.test({
  name: "PetApi uploadFile",
  fn: async () => {
    try {
      const blob = new Blob(
        [Deno.readFileSync("./test/api/pet.png")],
        { type: "application/octet-binary" },
      );
      const file = new File([blob], "pet.png");
      const response = await petApi.uploadFile(petId, "Metadata", file);
      assert(response.code && response.code >= 200 && response.code < 300);
      assert(response.message && response.message.indexOf("pet.png") >= 0);
    } catch (err) {
      fail(err);
    }
  },
});
