import * as petstore from 'ts-petstore-client';
import { expect} from "chai";



describe("Security Authentication", () => {
    describe("No Authentication", () => {
        it("No Authentication", () => {
            let ctx = new petstore.RequestContext("http://google.com", petstore.HttpMethod.GET);
            let noAuth = new petstore.NoAuthentication();
            noAuth.applySecurityAuthentication(ctx);
    
            expect(ctx.getUrl()).to.equal("http://google.com");
            expect(ctx.getHeaders()).to.deep.equal({});
            expect(ctx.getBody()).to.equal(undefined);
        });    
    })

    describe("API Key Authentication", () => {
        // TODO: make all params const variables
        it("Header API Key", () => {
            let ctx = new petstore.RequestContext("http://google.com", petstore.HttpMethod.GET);
            let auth = new petstore.APIKeyAuthentication("my_name", "paramName", "header", "apiKey");
            auth.applySecurityAuthentication(ctx);
    
            expect(ctx.getUrl()).to.equal("http://google.com");
            expect(ctx.getHeaders()).to.have.property("paramName");
            expect(ctx.getHeaders()["paramName"]).to.equal("apiKey");
            expect(ctx.getBody()).to.equal(undefined);
        });    

        it("Query API Key", () => {
            let ctx = new petstore.RequestContext("http://google.com?a=b", petstore.HttpMethod.GET);
            let auth = new petstore.APIKeyAuthentication("my_name", "paramName", "query", "apiKey",);
            auth.applySecurityAuthentication(ctx);
    
            expect(ctx.getUrl()).to.contain("paramName=apiKey");
            expect(ctx.getHeaders()).to.deep.equal({});
            expect(ctx.getBody()).to.equal(undefined);
        });    

        it("Cookie API Key", () => {
            let ctx = new petstore.RequestContext("http://google.com", petstore.HttpMethod.GET);
            let auth = new petstore.APIKeyAuthentication("my_name", "paramName", "cookie", "apiKey",);
            auth.applySecurityAuthentication(ctx);
    
            expect(ctx.getUrl()).to.equal("http://google.com");
            expect(ctx.getHeaders()).to.have.property("Cookie");
            expect(ctx.getHeaders()["Cookie"]).to.contain("paramName=apiKey; ");
            expect(ctx.getBody()).to.equal(undefined);
        });    

    })
});