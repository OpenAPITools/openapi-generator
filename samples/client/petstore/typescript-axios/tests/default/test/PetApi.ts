import { expect } from "chai";
import { PetApi, Pet, PetStatusEnum, Category } from "@openapitools/typescript-axios-petstore";
import axios, {AxiosInstance, AxiosResponse} from "@openapitools/typescript-axios-petstore/node_modules/axios";

describe("PetApi", () => {
  function runSuite(description: string, requestOptions?: any, customAxiosInstance?: AxiosInstance): void {
    describe(description, () => {
      let api: PetApi;
      const fixture: Pet = createTestFixture();

      beforeEach(() => {
        api = new PetApi(undefined, undefined, customAxiosInstance);
      });

      it("should add and delete Pet", () => {
        return api.addPet(fixture, requestOptions).then(() => {});
      });

      it("should get Pet by ID", () => {
        return api
          .getPetById(fixture.id, requestOptions)
          .then((result: AxiosResponse<Pet>) => {
            return expect(result.data).to.deep.equal(fixture);
          });
      });

      it("should update Pet by ID", () => {
        return api
          .getPetById(fixture.id, requestOptions)
          .then((response: AxiosResponse<Pet>) => {
            const result = response.data;
            result.name = "newname";
            return api.updatePet(result, requestOptions).then(() => {
              return api
                .getPetById(fixture.id, requestOptions)
                .then((response: AxiosResponse<Pet>) => {
                  return expect(response.data.name).to.deep.equal("newname");
                });
            });
          });
      });

      it("should delete Pet", () => {
        return api.deletePet(fixture.id, requestOptions);
      });

      it("should not contain deleted Pet", () => {
        return api.getPetById(fixture.id, requestOptions).then(
          (result: AxiosResponse<Pet>) => {
            return expect(result.data).to.not.exist;
          },
          (err: any) => {
            return expect(err).to.exist;
          }
        );
      });
    });
  }

  runSuite("without custom request options");

  runSuite("with custom request options", {
    credentials: "include",
    mode: "cors"
  });

  runSuite("without custom axios instance");

  runSuite("with custom axios instance",{}, axios);

  runSuite("with custom request options and custom axios instance",{
      credentials: "include",
      mode: "cors"
  }, axios);
});

function createTestFixture(ts = Date.now()) {
  const category: Category = {
    id: ts,
    name: `category${ts}`
  };

  const pet: Pet = {
    id: ts,
    name: `pet${ts}`,
    category: category,
    photoUrls: ["http://foo.bar.com/1", "http://foo.bar.com/2"],
    status: PetStatusEnum.Available,
    tags: []
  };

  return pet;
}
