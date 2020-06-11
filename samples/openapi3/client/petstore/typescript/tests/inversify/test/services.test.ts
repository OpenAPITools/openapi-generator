import "reflect-metadata";
import { Container } from "inversify";

import * as petstore from "ts-petstore-client";
import * as petstoreInternals from "ts-petstore-client/dist/apis/PetApi";

import { expect, assert } from "chai";
import * as fs from "fs";

describe("ApiServiceBinder", () => {
    let container: Container, apiServiceBinder: petstore.ApiServiceBinder;
    beforeEach("create new container and api service binder", () => {
        container = new Container();
        apiServiceBinder = new petstore.ApiServiceBinder(container);
    });

    it("falls back to defaults", () => {
        const petApi = container.resolve(petstore.PetApi);
        expect(petApi).to.be.an.instanceof(petstore.PetApi);
    });

    it("binds server config", async () => {
        const url = "foobar";
        let callCount = 0;
        const mockServer = {
            makeRequestContext(endpoint: string, httpMethod: petstore.HttpMethod): petstore.RequestContext {
                callCount++;
                return new petstore.RequestContext(url, httpMethod);
            }
        };
        apiServiceBinder.bindServerConfiguration.toConstantValue(mockServer);

        const requestFactory = container.resolve(petstoreInternals.PetApiRequestFactory);
        const request = await requestFactory.deletePet(42);

        expect(callCount).to.equal(1);
        expect(request.getUrl()).to.equal(url);
    });

    it("binds predefined server config", () => {
        apiServiceBinder.bindServerConfigurationToPredefined(0);
        const server = container.get(petstore.AbstractServerConfiguration);

        expect(server).to.equal(petstore.server1);
    });

    it("binds server config to url", async () => {
        const url = "foobar";
        const petId = 42;
        apiServiceBinder.bindServerConfigurationToURL(url);

        const requestFactory = container.resolve(petstoreInternals.PetApiRequestFactory);
        const request = await requestFactory.deletePet(petId);

        expect(request.getUrl()).to.equal(`${url}/pet/${petId}`);
    });

    it("binds http library", async () => {
        const requests: Array<petstore.RequestContext> = [];
        const mockHttpLibrary = {
            async send(request: petstore.RequestContext): Promise<petstore.ResponseContext> {
                requests.push(request);
                return new petstore.ResponseContext(200, {}, {
                    async text() { return ""; },
                    async binary() { return Buffer.from(""); }
                });
            }
        };
        apiServiceBinder.bindHttpLibrary.toConstantValue(mockHttpLibrary);

        const petApi = container.resolve(petstore.PetApi);
        await petApi.deletePet(42);

        expect(requests).to.be.of.length(1);
        expect(requests[0].getUrl()).to.include("/pet/42");
    });

    it("binds middleware", async () => {
        let preCallCount = 0, postCallCount = 0;
        const mockMiddleware = {
            async pre(context: petstore.RequestContext): Promise<petstore.RequestContext> {
                preCallCount++;
                return context;
            },
            async post(context: petstore.ResponseContext): Promise<petstore.ResponseContext> {
                postCallCount++;
                return context;
            }
        };
        apiServiceBinder.bindMiddleware.toConstantValue(mockMiddleware);

        const petApi = container.resolve(petstore.PetApi);
        await petApi.addPet({ name: "Foo Bear", photoUrls: [] });

        expect(preCallCount).to.equal(1);
        expect(postCallCount).to.equal(1);
    });

    it("binds auth method", async () => {
        const header = "x-custom-header";
        const value = "foobar";
        let callCount = 0;
        const mockAuthMethod = {
            getName(): string {
                return "api_key";
            },
            applySecurityAuthentication(context: petstore.RequestContext) {
                callCount++;
                context.setHeaderParam(header, value);
            }
        };
        apiServiceBinder.bindAuthMethod.toConstantValue(mockAuthMethod);

        const requestFactory = container.resolve(petstoreInternals.PetApiRequestFactory);
        const request = await requestFactory.getPetById(42);

        expect(callCount).to.equal(1);
        expect(request.getHeaders()[header]).to.equal(value);
    });

    it("binds predefined auth method", async () => {
        const authName = "api_key";
        const value = "foobar";
        container.bind(petstore.AuthApiKey).toConstantValue(value).whenTargetNamed(authName);
        apiServiceBinder.bindAuthMethodToPredefined(authName);

        const requestFactory = container.resolve(petstoreInternals.PetApiRequestFactory);
        const request = await requestFactory.getPetById(42);

        expect(request.getHeaders()["api_key"]).to.equal(value);
    });

    it("binds all api services", () => {
        apiServiceBinder.bindAllApiServices();
        const petApi = container.get(petstore.AbstractPetApi);

        expect(petApi).to.be.an.instanceof(petstore.PetApi);
    });
})