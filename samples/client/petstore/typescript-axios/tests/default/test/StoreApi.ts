import { expect } from "chai";
import { StoreApi } from "@openapitools/typescript-axios-petstore";
import axios, {AxiosInstance, AxiosResponse} from "@openapitools/typescript-axios-petstore/node_modules/axios";

describe("StoreApi", function() {
  function runSuite(description: string, requestOptions?: any, customAxiosInstance?: AxiosInstance): void {
    describe(description, () => {
      let api: StoreApi;

      beforeEach(function() {
        api = new StoreApi(undefined, undefined, customAxiosInstance);
      });

      it("should get inventory", function() {
        return api
          .getInventory(requestOptions)
          .then((result: AxiosResponse<{ [key: string]: number }>) => {
            expect(Object.keys(result)).to.not.be.empty;
          });
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
