"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var ts_petstore_client_1 = require("ts-petstore-client");
var ts_petstore_client_2 = require("ts-petstore-client");
var chai_1 = require("chai");
var FormData = require("form-data");
var libs = {
    "isomorphic-fetch": new ts_petstore_client_2.IsomorphicFetchHttpLibrary()
};
var _loop_1 = function (libName) {
    var lib = libs[libName];
    describe("Isomorphic Fetch", function () {
        it("GET-Request", function (done) {
            var requestContext = new ts_petstore_client_1.RequestContext("http://httpbin.org/get", ts_petstore_client_1.HttpMethod.GET);
            requestContext.setHeaderParam("X-Test-Token", "Test-Token");
            requestContext.addCookie("test-cookie", "cookie-value");
            lib.send(requestContext).then(function (resp) {
                chai_1.expect(resp.httpStatusCode, "Expected status code to be 200").to.eq(200);
                var body = JSON.parse(resp.body);
                chai_1.expect(body["headers"]).to.exist;
                chai_1.expect(body["headers"]["X-Test-Token"]).to.equal("Test-Token");
                chai_1.expect(body["headers"]["Cookie"]).to.equal("test-cookie=cookie-value;");
                done();
            }).catch(function (e) {
                done(e);
            });
        });
        it("POST-Request", function (done) {
            var requestContext = new ts_petstore_client_1.RequestContext("http://httpbin.org/post", ts_petstore_client_1.HttpMethod.POST);
            requestContext.setHeaderParam("X-Test-Token", "Test-Token");
            requestContext.addCookie("test-cookie", "cookie-value");
            var formData = new FormData();
            formData.append("test", "test2");
            formData.append("testFile", Buffer.from("abc"), "fileName.json");
            requestContext.setBody(formData);
            lib.send(requestContext).then(function (resp) {
                chai_1.expect(resp.httpStatusCode, "Expected status code to be 200").to.eq(200);
                var body = JSON.parse(resp.body);
                chai_1.expect(body["headers"]).to.exist;
                chai_1.expect(body["headers"]["X-Test-Token"]).to.equal("Test-Token");
                chai_1.expect(body["headers"]["Cookie"]).to.equal("test-cookie=cookie-value;");
                chai_1.expect(body["files"]["testFile"]).to.equal("abc");
                chai_1.expect(body["form"]["test"]).to.equal("test2");
                done();
            }).catch(function (e) {
                done(e);
            });
        });
        it("Cookies-Request", function (done) {
            var requestContext = new ts_petstore_client_1.RequestContext("http://httpbin.org/cookies", ts_petstore_client_1.HttpMethod.GET);
            requestContext.addCookie("test-cookie", "cookie-value");
            lib.send(requestContext).then(function (resp) {
                chai_1.expect(resp.httpStatusCode, "Expected status code to be 200").to.eq(200);
                var body = JSON.parse(resp.body);
                chai_1.expect(body["cookies"]["test-cookie"]).to.equal("cookie-value");
                done();
            }).catch(function (e) {
                done(e);
            });
        });
    });
};
for (var libName in libs) {
    _loop_1(libName);
}
