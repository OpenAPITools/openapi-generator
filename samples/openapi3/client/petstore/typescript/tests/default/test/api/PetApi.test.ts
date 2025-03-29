import * as petstore from 'ts-petstore-client'
import { Middleware, RequestContext, ResponseContext, wrapHttpLibrary } from 'ts-petstore-client'

import { expect } from "chai";
import * as fs from 'fs';

const configuration = petstore.createConfiguration()
const petApi = new petstore.PetApi(configuration)

const tag = new petstore.Tag();
tag.name = "tag1"
tag.id = Math.floor(Math.random() * 100000)

let pet: petstore.Pet;
function overridePetIDMiddleware(id: number): Middleware {
  return {
    pre: (c: RequestContext) => {
      return new Promise((resolve) => {
        const segments = c.getUrl().split('/')
        segments[segments.length - 1] = id.toString()
        const newURL = segments.join('/')
        c.setUrl(newURL)
        resolve(c)
      })
    },
    post: (c: ResponseContext) => {
      return new Promise<ResponseContext>((resolve) => {
        resolve(c)
      })
    },
  }
}

function NoopMiddleware(onPre: () => void, onPost: () => void): Middleware {
  return {
    pre: (c: RequestContext) => {
      return new Promise((resolve) => {
        onPre()
        resolve(c)
      })
    },
    post: (c: ResponseContext) => {
      return new Promise<ResponseContext>((resolve) => {
        onPost()
        resolve(c)
      })
    },
  }
}

function MiddlewareCallTracker() {
  let CallOrder = [] as string[]
  return {
    CallOrder,
    BaseMiddleware: NoopMiddleware(() => CallOrder.push('base-pre'), () => CallOrder.push('base-post')),
    CalltimeMiddleware: NoopMiddleware(() => CallOrder.push('call-pre'), () => CallOrder.push('call-post'))
  }
}

describe("PetApi", () => {
  beforeEach(async () => {
    pet = new petstore.Pet()
    pet.id = Math.floor(Math.random() * 100000)
    pet.name = "PetName"
    pet.photoUrls = []
    pet.status = petstore.PetStatusEnum.Available
    pet.tags = [tag]

    await petApi.addPet(pet);
  });

  it("addPet", async () => {
    const createdPet = await petApi.getPetById(pet.id)
    expect(createdPet).to.deep.equal(pet);
  })

  it("addPetViaMiddleware", async () => {
    const wrongId = pet.id + 1
    const createdPet = await petApi.getPetById(wrongId, { middleware: [overridePetIDMiddleware(pet.id)] })
    expect(createdPet).to.deep.equal(pet);
  })

  it("appendMiddleware petid", async () => {
    const wrongId = pet.id + 100
    const configuration = petstore.createConfiguration({ promiseMiddleware: [overridePetIDMiddleware(wrongId)] })
    const petApi = new petstore.PetApi(configuration)
    try {
      void await petApi.getPetById(pet.id)
    } catch (err) {
      expect(err.code).to.equal(404);
      expect(err.message).to.include("Pet not found");
    }
    const callTimeAppendedRightPet = await petApi.getPetById(wrongId, { middleware: [overridePetIDMiddleware(pet.id)], middlewareMergeStrategy: 'append' })
    expect(callTimeAppendedRightPet).to.deep.equal(pet);
  })

  it("should keep middleware when no options are given", async () => {
    let { CallOrder, BaseMiddleware, CalltimeMiddleware } = MiddlewareCallTracker()
    const configuration = petstore.createConfiguration({ promiseMiddleware: [BaseMiddleware] });
    const petApi = new petstore.PetApi(configuration);
    const callTimeAppendedRightPet = await petApi.getPetById(pet.id);
    expect(callTimeAppendedRightPet).to.deep.equal(pet);
    expect(CallOrder).deep.equal(['base-pre', 'base-post'])
  })

  it("should keep middleware when options are given without middleware", async () => {
    let { CallOrder, BaseMiddleware, CalltimeMiddleware } = MiddlewareCallTracker()
    const configuration = petstore.createConfiguration({ promiseMiddleware: [BaseMiddleware] });
    const petApi = new petstore.PetApi(configuration);
    const callTimeAppendedRightPet = await petApi.getPetById(pet.id, {});
    expect(callTimeAppendedRightPet).to.deep.equal(pet);
    expect(CallOrder).deep.equal(['base-pre', 'base-post'])
  })

  it("should replace middleware when options contain an empty array of middlewares", async () => {
    let { CallOrder, BaseMiddleware, CalltimeMiddleware } = MiddlewareCallTracker()
    const configuration = petstore.createConfiguration({ promiseMiddleware: [BaseMiddleware] });
    const petApi = new petstore.PetApi(configuration);
    const callTimeAppendedRightPet = await petApi.getPetById(pet.id, { middleware: [] });
    expect(callTimeAppendedRightPet).to.deep.equal(pet);
    expect(CallOrder).deep.equal([])
  })

  it("replace Middleware call order", async () => {
    let { CallOrder, BaseMiddleware, CalltimeMiddleware } = MiddlewareCallTracker()
    const configuration = petstore.createConfiguration({ promiseMiddleware: [BaseMiddleware] })
    const petApi = new petstore.PetApi(configuration)
    const callTimeAppendedRightPet = await petApi.getPetById(pet.id, { middleware: [CalltimeMiddleware] })
    expect(callTimeAppendedRightPet).to.deep.equal(pet);
    expect(CallOrder).deep.equal(['call-pre', 'call-post'])
  })

  it("prepend Middleware call order", async () => {
    let { CallOrder, BaseMiddleware, CalltimeMiddleware } = MiddlewareCallTracker()
    const configuration = petstore.createConfiguration({ promiseMiddleware: [BaseMiddleware] })
    const petApi = new petstore.PetApi(configuration)
    const callTimeAppendedRightPet = await petApi.getPetById(pet.id, { middleware: [CalltimeMiddleware], middlewareMergeStrategy: 'prepend' })
    expect(callTimeAppendedRightPet).to.deep.equal(pet);
    expect(CallOrder).deep.equal(['call-pre', 'base-pre', 'base-post','call-post'])
  })


  it("append Middleware call order", async () => {
    let { CallOrder, BaseMiddleware, CalltimeMiddleware } = MiddlewareCallTracker()
    const configuration = petstore.createConfiguration({ promiseMiddleware: [BaseMiddleware] })
    const petApi = new petstore.PetApi(configuration)
    const callTimeAppendedRightPet = await petApi.getPetById(pet.id, { middleware: [CalltimeMiddleware],middlewareMergeStrategy: 'append' })
    expect(callTimeAppendedRightPet).to.deep.equal(pet);
    expect(CallOrder).deep.equal(['base-pre','call-pre','call-post','base-post'])
  })


  it("prependMiddleware pet id", async () => {
    const wrongId = pet.id + 100
    const configuration = petstore.createConfiguration({ promiseMiddleware: [overridePetIDMiddleware(pet.id)] })
    const petApi = new petstore.PetApi(configuration)
    const callTimeAppendedRightPet = await petApi.getPetById(wrongId, { middleware: [overridePetIDMiddleware(wrongId)], middlewareMergeStrategy: 'prepend' })
    expect(callTimeAppendedRightPet).to.deep.equal(pet);
  })

  it("should override http api from option", async () => {
    const configuration = petstore.createConfiguration();
    const petApi = new petstore.PetApi(configuration);

    let overriddenCalls = 0;
    const mockHttpLibrary = {
      async send(): Promise<ResponseContext> {
        overriddenCalls++;
        return new ResponseContext(200, {
          "content-type": "application/json",
        }, {
          async text() {
            return "{}";
          },
          async binary() {
            throw new Error("Unexpected usage of mock body as binary.");
          }
        });
      }
    };

    await petApi.getPetById(pet.id, { httpApi: wrapHttpLibrary(mockHttpLibrary) });
    expect(overriddenCalls).to.equal(1);
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

  it("deleteNonExistentPet", async () => {
    // Use an id that is too big for the server to handle.
    const nonExistentId = 100000000000000000000000000;
    try {
      await petApi.deletePet(nonExistentId)
    } catch (err) {
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
      await petApi.deletePet(null)
    } catch (err) {
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
    const image = fs.readFileSync(__dirname + "/pet.png")
    const response = await petApi.uploadFile(pet.id, "Metadata", { name: "pet.png", data: image });
    expect(response.code).to.be.gte(200).and.lt(300);
    expect(response.message).to.contain("pet.png");
  })
})
