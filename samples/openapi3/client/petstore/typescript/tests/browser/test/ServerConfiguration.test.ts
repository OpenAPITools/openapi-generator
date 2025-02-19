import { expect } from '@esm-bundle/chai';
import { ServerConfiguration, HttpMethod } from 'ts-petstore-client'

describe("ServerConfiguration", () => {
    it("supports absolute http URLs", async () => {
        const config = new ServerConfiguration("http://localhost/v2", {});
        expect(config.makeRequestContext("/resource", HttpMethod.PUT).getUrl()).to.equal("http://localhost/v2/resource");
    })

    it("supports absolute https URLs", async () => {
        const config = new ServerConfiguration("https://localhost/v2", {});
        expect(config.makeRequestContext("/resource", HttpMethod.PUT).getUrl()).to.equal("https://localhost/v2/resource");
    })

    it("supports relative URLs", async () => {
        const config = new ServerConfiguration("/api", {});
        expect(config.makeRequestContext("/resource", HttpMethod.PUT).getUrl()).to.equal("http://localhost:8080/api/resource");
    })
})
