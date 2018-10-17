"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var ts_petstore_client_1 = require("ts-petstore-client");
var ts_petstore_client_2 = require("ts-petstore-client");
var chai_1 = require("chai");
describe("Security Authentication", function () {
    describe("No Authentication", function () {
        it("No Authentication", function () {
            var ctx = new ts_petstore_client_2.RequestContext("http://google.com", ts_petstore_client_2.HttpMethod.GET);
            var noAuth = new ts_petstore_client_1.NoAuthentication();
            noAuth.applySecurityAuthentication(ctx);
            chai_1.expect(ctx.getUrl()).to.equal("http://google.com");
            chai_1.expect(ctx.getHeaders()).to.deep.equal({});
            chai_1.expect(ctx.getBody()).to.equal("");
        });
    });
    describe("API Key Authentication", function () {
        // TODO: make all params const variables
        it("Header API Key", function () {
            var ctx = new ts_petstore_client_2.RequestContext("http://google.com", ts_petstore_client_2.HttpMethod.GET);
            var auth = new ts_petstore_client_1.APIKeyAuthentication("my_name", "paramName", "header", "apiKey");
            auth.applySecurityAuthentication(ctx);
            chai_1.expect(ctx.getUrl()).to.equal("http://google.com");
            chai_1.expect(ctx.getHeaders()).to.have.property("paramName");
            chai_1.expect(ctx.getHeaders()["paramName"]).to.equal("apiKey");
            chai_1.expect(ctx.getBody()).to.equal("");
        });
        it("Query API Key", function () {
            var ctx = new ts_petstore_client_2.RequestContext("http://google.com?a=b", ts_petstore_client_2.HttpMethod.GET);
            var auth = new ts_petstore_client_1.APIKeyAuthentication("my_name", "paramName", "query", "apiKey");
            auth.applySecurityAuthentication(ctx);
            chai_1.expect(ctx.getUrl()).to.contain("paramName=apiKey");
            chai_1.expect(ctx.getHeaders()).to.deep.equal({});
            chai_1.expect(ctx.getBody()).to.equal("");
        });
        it("Cookie API Key", function () {
            var ctx = new ts_petstore_client_2.RequestContext("http://google.com", ts_petstore_client_2.HttpMethod.GET);
            var auth = new ts_petstore_client_1.APIKeyAuthentication("my_name", "paramName", "cookie", "apiKey");
            auth.applySecurityAuthentication(ctx);
            chai_1.expect(ctx.getUrl()).to.equal("http://google.com");
            chai_1.expect(ctx.getHeaders()).to.have.property("Cookie");
            chai_1.expect(ctx.getHeaders()["Cookie"]).to.contain("paramName=apiKey; ");
            chai_1.expect(ctx.getBody()).to.equal("");
        });
    });
});
