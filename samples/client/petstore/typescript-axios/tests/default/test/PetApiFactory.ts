import { expect } from "chai";
import {
  PetApiFactory,
  Pet,
  PetStatusEnum,
  Category
} from "@openapitools/typescript-axios-petstore";
import { Configuration } from "@openapitools/typescript-axios-petstore";
import axios, {AxiosInstance, AxiosResponse} from "@openapitools/typescript-axios-petstore/node_modules/axios";

let config: Configuration;

before(function() {
  config = new Configuration();
  config.accessToken = "foobar";
  config.apiKey = (securityName: string) => {
    // for multiple apiKey security
    if (securityName === "api_key") {
      return "foobar";
    }
    return;
  };
  config.username = "foo";
  config.password = "bar";
});

describe("PetApiFactory", () => {
  function runSuite(description: string, requestOptions?: any, customAxiosInstance?: AxiosInstance): void {
    describe(description, () => {
      const fixture: Pet = createTestFixture();

      it("should add and delete Pet", () => {
        return PetApiFactory(config, undefined, customAxiosInstance)
          .addPet(fixture, requestOptions)
          .then(() => {});
      });

      it("should get Pet by ID", () => {
        return PetApiFactory(config, undefined, customAxiosInstance)
          .getPetById(fixture.id, requestOptions)
          .then((result: AxiosResponse<Pet>) => {
            return expect(result.data).to.deep.equal(fixture);
          });
      });

      it("should update Pet by ID", () => {
        return PetApiFactory(config, undefined, customAxiosInstance)
          .getPetById(fixture.id, requestOptions)
          .then((result: AxiosResponse<Pet>) => {
            result.data.name = "newname";
            return PetApiFactory(config)
              .updatePet(result.data, requestOptions)
              .then(() => {
                return PetApiFactory(config)
                  .getPetById(fixture.id, requestOptions)
                  .then((result: AxiosResponse<Pet>) => {
                    return expect(result.data.name).to.deep.equal("newname");
                  });
              });
          });
      });

      it("should delete Pet", () => {
        return PetApiFactory(config, undefined, customAxiosInstance).deletePet(fixture.id, requestOptions);
      });

      it("should not contain deleted Pet", () => {
        return PetApiFactory(config, undefined, customAxiosInstance)
          .getPetById(fixture.id, requestOptions)
          .then(
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
