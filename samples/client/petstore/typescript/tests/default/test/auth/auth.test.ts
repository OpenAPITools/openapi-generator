import {NoAuthentication, APIKeyAuthentication} from "ts-petstore-client";
import {RequestContext, HttpMethod} from 'ts-petstore-client';
import { expect} from "chai";



describe("Security Authentication", () => {
    describe("No Authentication", () => {
        it("No Authentication", () => {
            let ctx = new RequestContext("http://google.com", HttpMethod.GET);
            let noAuth = new NoAuthentication();
            noAuth.applySecurityAuthentication(ctx);
    
            expect(ctx.getUrl()).to.equal("http://google.com");
            expect(ctx.getHeaders()).to.deep.equal({});
            expect(ctx.getBody()).to.equal("");
        });    
    })

    describe("API Key Authentication", () => {
        // TODO: make all params const variables
        it("Header API Key", () => {
            let ctx = new RequestContext("http://google.com", HttpMethod.GET);
            let auth = new APIKeyAuthentication("my_name", "paramName", "header", "apiKey");
            auth.applySecurityAuthentication(ctx);
    
            expect(ctx.getUrl()).to.equal("http://google.com");
            expect(ctx.getHeaders()).to.have.property("paramName");
            expect(ctx.getHeaders()["paramName"]).to.equal("apiKey");
            expect(ctx.getBody()).to.equal("");
        });    

        it("Query API Key", () => {
            let ctx = new RequestContext("http://google.com?a=b", HttpMethod.GET);
            let auth = new APIKeyAuthentication("my_name", "paramName", "query", "apiKey",);
            auth.applySecurityAuthentication(ctx);
    
            expect(ctx.getUrl()).to.contain("paramName=apiKey");
            expect(ctx.getHeaders()).to.deep.equal({});
            expect(ctx.getBody()).to.equal("");
        });    

        it("Cookie API Key", () => {
            let ctx = new RequestContext("http://google.com", HttpMethod.GET);
            let auth = new APIKeyAuthentication("my_name", "paramName", "cookie", "apiKey",);
            auth.applySecurityAuthentication(ctx);
    
            expect(ctx.getUrl()).to.equal("http://google.com");
            expect(ctx.getHeaders()).to.have.property("Cookie");
            expect(ctx.getHeaders()["Cookie"]).to.contain("paramName=apiKey; ");
            expect(ctx.getBody()).to.equal("");
        });    

    })
});