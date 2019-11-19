import * as petstore from "ts-petstore-client";

import { expect} from "chai";
import * as FormData from "form-data";

let libs: { [key: string]: petstore.http.HttpLibrary } = {
    "isomorphic-fetch": new petstore.http.IsomorphicFetchHttpLibrary()
}


for (let libName in libs) {
    let lib = libs[libName];

    describe("Isomorphic Fetch", () => {
        it("GET-Request", (done) => {
            let requestContext = new petstore.http.RequestContext("http://httpbin.org/get", petstore.http.HttpMethod.GET);
            requestContext.setHeaderParam("X-Test-Token", "Test-Token");
            requestContext.addCookie("test-cookie", "cookie-value");
            lib.send(requestContext).toPromise().then((resp: petstore.http.ResponseContext) => {
                    expect(resp.httpStatusCode, "Expected status code to be 200").to.eq(200);
                    let body = JSON.parse(resp.body);
                    expect(body["headers"]).to.exist;
                    expect(body["headers"]["X-Test-Token"]).to.equal("Test-Token");
                    expect(body["headers"]["Cookie"]).to.equal("test-cookie=cookie-value;");
                    done();
                },
                (e: any) => {
                    done(e);
                }
                )
        })

        it("POST-Request", (done) => {
            let requestContext = new petstore.http.RequestContext("http://httpbin.org/post", petstore.http.HttpMethod.POST);
            requestContext.setHeaderParam("X-Test-Token", "Test-Token");
            requestContext.addCookie("test-cookie", "cookie-value");
            let formData: FormData = new FormData()
            formData.append("test", "test2");
            formData.append("testFile", Buffer.from("abc"), "fileName.json");

            requestContext.setBody(formData);
            lib.send(requestContext).toPromise().then(
                    (resp: petstore.http.ResponseContext) => {
                    expect(resp.httpStatusCode, "Expected status code to be 200").to.eq(200);
                    let body = JSON.parse(resp.body);
                    expect(body["headers"]).to.exist;
                    expect(body["headers"]["X-Test-Token"]).to.equal("Test-Token");
                    expect(body["headers"]["Cookie"]).to.equal("test-cookie=cookie-value;");
                    expect(body["files"]["testFile"]).to.equal("abc");
                    expect(body["form"]["test"]).to.equal("test2");
                    done();
                },
                (e: any) => {
                    done(e);
                })            
        });

        it("Cookies-Request", (done) => {
            let requestContext = new petstore.http.RequestContext("http://httpbin.org/cookies", petstore.http.HttpMethod.GET);
            requestContext.addCookie("test-cookie", "cookie-value");

            lib.send(requestContext).toPromise().then(
                (resp: petstore.http.ResponseContext) => {
                    expect(resp.httpStatusCode, "Expected status code to be 200").to.eq(200);
                    let body = JSON.parse(resp.body);
                    expect(body["cookies"]["test-cookie"]).to.equal("cookie-value");
                    done();
                },
                (e: any) => {
                    done(e);
                })            
        })
    })
}