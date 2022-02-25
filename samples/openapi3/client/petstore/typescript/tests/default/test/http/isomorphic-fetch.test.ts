import * as petstore from "ts-petstore-client";

import { expect} from "chai";
import * as FormData from "form-data";

let libs: { [key: string]: petstore.HttpLibrary } = {
    "isomorphic-fetch": new petstore.IsomorphicFetchHttpLibrary()
}


for (let libName in libs) {
    let lib = libs[libName];

    // Tests disabled due to error on circle CI
    xdescribe("Isomorphic Fetch", () => {
        it("GET-Request", (done) => {
            let requestContext = new petstore.RequestContext("http://httpbin.org/get", petstore.HttpMethod.GET);
            requestContext.setHeaderParam("X-Test-Token", "Test-Token");
            requestContext.addCookie("test-cookie", "cookie-value");
            lib.send(requestContext).toPromise().then((resp: petstore.ResponseContext) => {
                    expect(resp.httpStatusCode, "Expected status code to be 200").to.eq(200);
                    return resp.body.text();
            }).then((bodyText: string) => {
                    let body = JSON.parse(bodyText);
                    expect(body["headers"]).to.exist;
                    expect(body["headers"]["X-Test-Token"]).to.equal("Test-Token");
                    expect(body["headers"]["Cookie"]).to.equal("test-cookie=cookie-value;");
                    done();
            }).catch(done)
        });

        it("POST-Request", (done) => {
            let requestContext = new petstore.RequestContext("http://httpbin.org/post", petstore.HttpMethod.POST);
            requestContext.setHeaderParam("X-Test-Token", "Test-Token");
            requestContext.addCookie("test-cookie", "cookie-value");
            let formData: FormData = new FormData()
            formData.append("test", "test2");
            formData.append("testFile", Buffer.from("abc"), "fileName.json");

            requestContext.setBody(formData);
            lib.send(requestContext).toPromise().then(
                    (resp: petstore.ResponseContext) => {
                    expect(resp.httpStatusCode, "Expected status code to be 200").to.eq(200);
                    return resp.body.text();
            }).then((bodyText: string) => {
                    let body = JSON.parse(bodyText);
                    expect(body["headers"]).to.exist;
                    expect(body["headers"]["X-Test-Token"]).to.equal("Test-Token");
                    expect(body["headers"]["Cookie"]).to.equal("test-cookie=cookie-value;");
                    expect(body["files"]["testFile"]).to.equal("abc");
                    expect(body["form"]["test"]).to.equal("test2");
                    done();
            }).catch(done)
        });

        it("Cookies-Request", (done) => {
            let requestContext = new petstore.RequestContext("http://httpbin.org/cookies", petstore.HttpMethod.GET);
            requestContext.addCookie("test-cookie", "cookie-value");

            lib.send(requestContext).toPromise().then(
                (resp: petstore.ResponseContext) => {
                    expect(resp.httpStatusCode, "Expected status code to be 200").to.eq(200);
                    return resp.body.text();
            }).then((bodyText: string) => {
                    let body = JSON.parse(bodyText);
                    expect(body["cookies"]["test-cookie"]).to.equal("cookie-value");
                    done();
            }).catch(done)
        })
    })
}