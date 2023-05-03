/*
// commenting out below tests for the time being since the requests consistently timeout

import { expect } from '@esm-bundle/chai';
import * as petstore from "ts-petstore-client";

let libs: { [key: string]: petstore.HttpLibrary } = {
    "isomorphic-fetch": new petstore.IsomorphicFetchHttpLibrary()
}

for (let libName in libs) {
    let lib = libs[libName];

    describe("Isomorphic Fetch", () => {
        it("GET-Request", (done) => {
            let requestContext = new petstore.RequestContext("http://httpbin.org/get", petstore.HttpMethod.GET);
            requestContext.setHeaderParam("X-Test-Token", "Test-Token");
            lib.send(requestContext).toPromise().then((resp: petstore.ResponseContext) => {
                    expect(resp.httpStatusCode, "Expected status code to be 200").to.eq(200);
                    return resp.body.text();
            }).then((bodyText: string) => {
                    let body = JSON.parse(bodyText);
                    expect(body["headers"]).to.exist;
                    expect(body["headers"]["X-Test-Token"]).to.equal("Test-Token");
                    done();
            }).catch(done)
        });

        it("POST-Request", (done) => {
            let requestContext = new petstore.RequestContext("http://httpbin.org/post", petstore.HttpMethod.POST);
            requestContext.setHeaderParam("X-Test-Token", "Test-Token");
            let formData: FormData = new FormData()
            formData.append("test", "test2");
            formData.append("testFile", new Blob(["abc"]), "fileName.json");

            requestContext.setBody(formData);
            lib.send(requestContext).toPromise().then(
                    (resp: petstore.ResponseContext) => {
                    expect(resp.httpStatusCode, "Expected status code to be 200").to.eq(200);
                    return resp.body.text();
            }).then((bodyText: string) => {
                    let body = JSON.parse(bodyText);
                    expect(body["headers"]).to.exist;
                    expect(body["headers"]["X-Test-Token"]).to.equal("Test-Token");
                    expect(body["files"]["testFile"]).to.equal("abc");
                    expect(body["form"]["test"]).to.equal("test2");
                    done();
            }).catch(done)
        });
    })
}
*/
