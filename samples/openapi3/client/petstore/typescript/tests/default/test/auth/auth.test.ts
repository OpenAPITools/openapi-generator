import * as petstore from 'ts-petstore-client';
import { expect } from "chai";



describe("Security Authentication", () => {
    describe("API Key Authentication", () => {
        // TODO: make all params const variables
        it("Header API Key", () => {
            let ctx = new petstore.RequestContext("http://google.com", petstore.HttpMethod.GET);
            let auth = new petstore.ApiKeyAuthentication("apiKey");
            auth.applySecurityAuthentication(ctx);

            expect(ctx.getUrl()).to.equal("http://google.com");
            expect(ctx.getHeaders()).to.have.property("api_key");
            expect(ctx.getHeaders()["api_key"]).to.equal("apiKey");
            expect(ctx.getBody()).to.equal(undefined);
        });

    })
});