import { RequestContext } from '../http/http';
export declare abstract class SecurityAuthentication {
    private name;
    constructor(name: string);
    getName(): string;
    abstract applySecurityAuthentication(context: RequestContext): void;
}
export declare class NoAuthentication extends SecurityAuthentication {
    constructor();
    applySecurityAuthentication(_context: RequestContext): void;
}
export declare class APIKeyAuthentication extends SecurityAuthentication {
    private paramName;
    private keyLocation;
    private apiKey;
    constructor(authName: string, paramName: string, keyLocation: "query" | "header" | "cookie", apiKey: string);
    applySecurityAuthentication(context: RequestContext): void;
}
export declare class HttpBasicAuthentication extends SecurityAuthentication {
    private username;
    private password;
    constructor(authName: string, username: string, password: string);
    applySecurityAuthentication(context: RequestContext): void;
}
export declare class OAuth2Authentication extends SecurityAuthentication {
    constructor(authName: string);
    applySecurityAuthentication(context: RequestContext): void;
}
export declare type AuthMethods = {
    "api_key"?: APIKeyAuthentication;
    "petstore_auth"?: OAuth2Authentication;
};
export declare type ApiKeyConfiguration = string;
export declare type HttpBasicConfiguration = {
    "username": string;
    "password": string;
};
export declare type OAuth2Configuration = string;
export declare type AuthMethodsConfiguration = {
    "api_key"?: ApiKeyConfiguration;
    "petstore_auth"?: OAuth2Configuration;
};
export declare function configureAuthMethods(conf: AuthMethodsConfiguration | undefined): AuthMethods;
