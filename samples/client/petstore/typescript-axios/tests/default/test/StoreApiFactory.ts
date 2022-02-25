import { expect } from "chai";
import { StoreApiFactory } from "@openapitools/typescript-axios-petstore";
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

describe("StoreApiFactory", function() {
  function runSuite(description: string, requestOptions?: any, customAxiosInstance?: AxiosInstance): void {
    describe(description, () => {
      it("should get inventory", function() {
        return StoreApiFactory(config, undefined, customAxiosInstance)
          .getInventory(requestOptions)
          .then((result: AxiosResponse<{ [key: string]: number }>) => {
            expect(Object.keys(result.data)).to.not.be.empty;
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
