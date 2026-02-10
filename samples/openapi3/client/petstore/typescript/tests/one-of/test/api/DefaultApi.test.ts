import * as petstore from 'ts-petstore-client'
import { BASE_URL } from './server';
import { expect } from "chai";

const server = new petstore.ServerConfiguration(BASE_URL, {})
const configuration = petstore.createConfiguration({
    baseServer: server
})
const DefaultApi = new petstore.DefaultApi(configuration)


describe("Test oneOf API methods", () => {

    it("Without discriminator", async () => {
        const response = await DefaultApi.testWithoutDiscriminator();
        expect(response).to.be.instanceof(petstore.Cat);
    })

    it("With discriminator", async () => {
        const response = await DefaultApi.testDiscriminator();
        expect(response).to.be.instanceof(petstore.Dog);
    })
})