import {RequestContext, HttpMethod, ResponseContext, HttpLibrary, IsomorphicFetchHttpLibrary } from "ts-petstore-client";
import { expect} from "chai";
import * as FormData from "form-data";

let libs: { [key: string]: HttpLibrary } = {
    "isomorphic-fetch": new IsomorphicFetchHttpLibrary()
}


for (let libName in libs) {
    let lib = libs[libName];

    describe("Isomorphic Fetch", () => {
        it("GET-Request", (done) => {
            let requestContext = new RequestContext("http://httpbin.org/get", HttpMethod.GET);
            requestContext.setHeaderParam("X-Test-Token", "Test-Token");
            requestContext.addCookie("test-cookie", "cookie-value");
            lib.send(requestContext).then((resp: ResponseContext) => {
                expect(resp.httpStatusCode, "Expected status code to be 200").to.eq(200);
                let body = JSON.parse(resp.body);
                expect(body["headers"]).to.exist;
                expect(body["headers"]["X-Test-Token"]).to.equal("Test-Token");
                expect(body["headers"]["Cookie"]).to.equal("test-cookie=cookie-value;");
                done();
            }).catch((e) => {
                done(e);
            })
        })

        it("POST-Request", (done) => {
            let requestContext = new RequestContext("http://httpbin.org/post", HttpMethod.POST);
            requestContext.setHeaderParam("X-Test-Token", "Test-Token");
            requestContext.addCookie("test-cookie", "cookie-value");
            let formData: FormData = new FormData()
            formData.append("test", "test2");
            formData.append("testFile", Buffer.from("abc"), "fileName.json");

            requestContext.setBody(formData);
            lib.send(requestContext).then((resp: ResponseContext) => {
                expect(resp.httpStatusCode, "Expected status code to be 200").to.eq(200);
                let body = JSON.parse(resp.body);
                expect(body["headers"]).to.exist;
                expect(body["headers"]["X-Test-Token"]).to.equal("Test-Token");
                expect(body["headers"]["Cookie"]).to.equal("test-cookie=cookie-value;");
                expect(body["files"]["testFile"]).to.equal("abc");
                expect(body["form"]["test"]).to.equal("test2");
                done();
            }).catch((e) => {
                done(e);
            })            
        });

        it("Cookies-Request", (done) => {
            let requestContext = new RequestContext("http://httpbin.org/cookies", HttpMethod.GET);
            requestContext.addCookie("test-cookie", "cookie-value");

            lib.send(requestContext).then((resp: ResponseContext) => {
                expect(resp.httpStatusCode, "Expected status code to be 200").to.eq(200);
                let body = JSON.parse(resp.body);
                expect(body["cookies"]["test-cookie"]).to.equal("cookie-value");
                done();
            }).catch((e) => {
                done(e);
            })            
        })
    })
}