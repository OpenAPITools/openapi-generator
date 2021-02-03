declare var QUnit: any;

import * as petstore from "ts-petstore-client";

let libs: { [key: string]: petstore.HttpLibrary } = {
    "jquery": new petstore.JQueryHttpLibrary()
}


for (let libName in libs) {
    let lib = libs[libName];

    QUnit.module(libName);
    QUnit.test("GET-Request", (assert: any) => {
        let requestContext = new petstore.RequestContext("http://httpbin.org/get", petstore.HttpMethod.GET);
        requestContext.setHeaderParam("X-Test-Token", "Test-Token");
        return new Promise((resolve, reject) => {
            lib.send(requestContext).toPromise().then((resp: petstore.ResponseContext) => {
                assert.ok(resp.httpStatusCode, 200, "Expected status code to be 200");
                return resp.body.text();
            }).then((txtBody: string) => {
                let body = JSON.parse(txtBody);
                assert.ok(body["headers"]);
                assert.equal(body["headers"]["X-Test-Token"],"Test-Token");
                resolve()    
            },
            (e: any) => {
                console.error(e)
                assert.ok(false, "Error: " + JSON.stringify(e))
                reject(e)
            })
        });
    })

    QUnit.test("POST-Request", (assert: any) => {
        let requestContext = new petstore.RequestContext("http://httpbin.org/post", petstore.HttpMethod.POST);
        requestContext.setHeaderParam("X-Test-Token", "Test-Token");
        let formData: FormData = new FormData()
        formData.append("test", "test2");
        formData.append("testFile", new Blob(["abc"], {
            type: 'text/plain'
        }), "fileName.json");

        requestContext.setBody(formData);
        return new Promise((resolve, reject) => {
            lib.send(requestContext).toPromise().then(
                (resp: petstore.ResponseContext) => {
                assert.ok(resp.httpStatusCode, 200, "Expected status code to be 200");
                return resp.body.text();
            }).then((txtBody: any) => {
                let body = JSON.parse(txtBody);
                assert.ok(body["headers"]);
                assert.equal(body["headers"]["X-Test-Token"], "Test-Token");
                assert.equal(body["files"]["testFile"], "abc");
                assert.equal(body["form"]["test"], "test2");
                resolve();    
            },
            (e: any) => {
                console.error(e)
                assert.ok(false, "Error: " + JSON.stringify(e))
                reject(e);
            })
        });            
    });

    QUnit.test("Cookie-Test", (assert: any) => {
        let requestContext = new petstore.RequestContext("http://httpbin.org/post", petstore.HttpMethod.POST);
        requestContext.addCookie("test", "test2");
        return new Promise((resolve, reject) => {
            try {
                lib.send(requestContext).toPromise().then(
                    (resp: petstore.ResponseContext) => {
                    assert.ok(false, "Expected this request to fail!")
                    reject("Successful request with Cookie Header!")
                },
                (e: any) => {
                    console.error(e)
                    assert.ok(false, "Request with cookie header field was sent and failed.")
                    reject(e);
                })
            } catch (e) {
                assert.equal(e.message, "Setting the \"Cookie\"-Header field is blocked by every major browser when using jquery.ajax requests. Please switch to another library like fetch to enable this option", "Received error when trying to send request containing Cookie Header with jquery");
                resolve();
            }
        });            
    });
}