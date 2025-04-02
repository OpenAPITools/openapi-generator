import { RequestContext } from "../http/http";

/**
 * Interface authentication schemes.
 */
export interface SecurityAuthentication {
    /*
     * @return returns the name of the security authentication as specified in OAI
     */
    getName(): string;

    /**
     * Applies the authentication scheme to the request context
     *
     * @params context the request context which should use this authentication scheme
     */
    applySecurityAuthentication(context: RequestContext): void | Promise<void>;
}

export interface TokenProvider {
  getToken(): Promise<string> | string;
}

/**
 * Applies oauth2 authentication to the request context.
 */
export class PetstoreAuthAuthentication implements SecurityAuthentication {
    /**
     * Configures OAuth2 with the necessary properties
     *
     * @param accessToken: The access token to be used for every request
     */
    public constructor(private accessToken: string) {}

    public getName(): string {
        return "petstore_auth";
    }

    public applySecurityAuthentication(context: RequestContext) {
        context.setHeaderParam("Authorization", "Bearer " + this.accessToken);
    }
}

/**
 * Applies apiKey authentication to the request context.
 */
export class ApiKeyAuthentication implements SecurityAuthentication {
    /**
     * Configures this api key authentication with the necessary properties
     *
     * @param apiKey: The api key to be used for every request
     */
    public constructor(private apiKey: string) {}

    public getName(): string {
        return "api_key";
    }

    public applySecurityAuthentication(context: RequestContext) {
        context.setHeaderParam("api_key", this.apiKey);
    }
}

/**
 * Applies apiKey authentication to the request context.
 */
export class ApiKeyQueryAuthentication implements SecurityAuthentication {
    /**
     * Configures this api key authentication with the necessary properties
     *
     * @param apiKey: The api key to be used for every request
     */
    public constructor(private apiKey: string) {}

    public getName(): string {
        return "api_key_query";
    }

    public applySecurityAuthentication(context: RequestContext) {
        context.setQueryParam("api_key_query", this.apiKey);
    }
}

/**
 * Applies http authentication to the request context.
 */
export class HttpBasicTestAuthentication implements SecurityAuthentication {
    /**
     * Configures the http authentication with the required details.
     *
     * @param username username for http basic authentication
     * @param password password for http basic authentication
     */
    public constructor(
        private username: string,
        private password: string
    ) {}

    public getName(): string {
        return "http_basic_test";
    }

    public applySecurityAuthentication(context: RequestContext) {
        let comb = Buffer.from(this.username + ":" + this.password, 'binary').toString('base64');
        context.setHeaderParam("Authorization", "Basic " + comb);
    }
}

/**
 * Applies http authentication to the request context.
 */
export class BearerTestAuthentication implements SecurityAuthentication {
    /**
     * Configures the http authentication with the required details.
     *
     * @param tokenProvider service that can provide the up-to-date token when needed
     */
    public constructor(private tokenProvider: TokenProvider) {}

    public getName(): string {
        return "bearer_test";
    }

    public async applySecurityAuthentication(context: RequestContext) {
        context.setHeaderParam("Authorization", "Bearer " + await this.tokenProvider.getToken());
    }
}

/**
 * Applies http authentication to the request context.
 */
export class HttpSignatureTestAuthentication implements SecurityAuthentication {

    public getName(): string {
        return "http_signature_test";
    }

    public applySecurityAuthentication(context: RequestContext) {
    }
}


export type AuthMethods = {
    "default"?: SecurityAuthentication,
    "petstore_auth"?: SecurityAuthentication,
    "api_key"?: SecurityAuthentication,
    "api_key_query"?: SecurityAuthentication,
    "http_basic_test"?: SecurityAuthentication,
    "bearer_test"?: SecurityAuthentication,
    "http_signature_test"?: SecurityAuthentication
}

export type ApiKeyConfiguration = string;
export type HttpBasicConfiguration = { "username": string, "password": string };
export type HttpBearerConfiguration = { tokenProvider: TokenProvider };
export type OAuth2Configuration = { accessToken: string };
export type HttpSignatureConfiguration = unknown; // TODO: Implement

export type AuthMethodsConfiguration = {
    "default"?: SecurityAuthentication,
    "petstore_auth"?: OAuth2Configuration,
    "api_key"?: ApiKeyConfiguration,
    "api_key_query"?: ApiKeyConfiguration,
    "http_basic_test"?: HttpBasicConfiguration,
    "bearer_test"?: HttpBearerConfiguration,
    "http_signature_test"?: HttpSignatureConfiguration
}

/**
 * Creates the authentication methods from a swagger description.
 *
 */
export function configureAuthMethods(config: AuthMethodsConfiguration | undefined): AuthMethods {
    let authMethods: AuthMethods = {}

    if (!config) {
        return authMethods;
    }
    authMethods["default"] = config["default"]

    if (config["petstore_auth"]) {
        authMethods["petstore_auth"] = new PetstoreAuthAuthentication(
            config["petstore_auth"]["accessToken"]
        );
    }

    if (config["api_key"]) {
        authMethods["api_key"] = new ApiKeyAuthentication(
            config["api_key"]
        );
    }

    if (config["api_key_query"]) {
        authMethods["api_key_query"] = new ApiKeyQueryAuthentication(
            config["api_key_query"]
        );
    }

    if (config["http_basic_test"]) {
        authMethods["http_basic_test"] = new HttpBasicTestAuthentication(
            config["http_basic_test"]["username"],
            config["http_basic_test"]["password"]
        );
    }

    if (config["bearer_test"]) {
        authMethods["bearer_test"] = new BearerTestAuthentication(
            config["bearer_test"]["tokenProvider"]
        );
    }

    if (config["http_signature_test"]) {
        authMethods["http_signature_test"] = new HttpSignatureTestAuthentication(
        );
    }

    return authMethods;
}