import type { AbstractServerConfiguration } from "./http";
import type { HttpLibrary, RequestContext } from "../http/http";
import type { Middleware } from "../middleware";
import type { AuthMethods, TokenProvider } from "../auth/auth";
import type { Configuration } from "../configuration";

export abstract class AbstractConfiguration implements Configuration {
    abstract get baseServer(): AbstractServerConfiguration;
    abstract get httpApi(): HttpLibrary;
    abstract get middleware(): Middleware[];
    abstract get authMethods(): AuthMethods;
}

export abstract class AbstractAuthMethod {
    public abstract getName(): string;
    public abstract applySecurityAuthentication(context: RequestContext): void | Promise<void>;
};

export abstract class AbstractTokenProvider implements TokenProvider {
    public abstract getToken(): string | Promise<string>;
}
