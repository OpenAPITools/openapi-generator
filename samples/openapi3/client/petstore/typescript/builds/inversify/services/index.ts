import { inject, injectable, multiInject, optional, interfaces } from "inversify";

import { Configuration } from "../configuration";
import { ServerConfiguration, servers } from "../servers";
import { HttpLibrary, wrapHttpLibrary } from "../http/http";
import { Middleware, PromiseMiddlewareWrapper } from "../middleware";
import { authMethodServices, AuthMethods } from "../auth/auth";

import { IsomorphicFetchHttpLibrary as DefaultHttpLibrary } from "../http/isomorphic-fetch";

import { AbstractHttpLibrary, AbstractMiddleware, AbstractServerConfiguration } from "./http";
import { AbstractConfiguration, AbstractAuthMethod, AbstractTokenProvider } from "./configuration";

export { AbstractHttpLibrary, AbstractMiddleware, AbstractServerConfiguration, AbstractConfiguration, AbstractAuthMethod, AbstractTokenProvider };

import * as apis from "../types/PromiseAPI";
import * as apiServices from "./PromiseAPI";

@injectable()
class InjectableConfiguration implements AbstractConfiguration {
    public httpApi: HttpLibrary = new DefaultHttpLibrary();
    public middleware: Middleware[] = [];
    public authMethods: AuthMethods = {};

    constructor(
        @inject(AbstractServerConfiguration) @optional() public baseServer: AbstractServerConfiguration = servers[0],
        @inject(AbstractHttpLibrary) @optional() httpApi: AbstractHttpLibrary,
        @multiInject(AbstractMiddleware) @optional() middleware: AbstractMiddleware[] = [],
        @multiInject(AbstractAuthMethod) @optional() securityConfiguration: AbstractAuthMethod[] = []
    ) {
        this.httpApi = httpApi === undefined ? new DefaultHttpLibrary() : wrapHttpLibrary(httpApi);
        for (const _middleware of middleware) {
            this.middleware.push(new PromiseMiddlewareWrapper(_middleware));
        }
        for (const authMethod of securityConfiguration) {
            const authName = authMethod.getName();
            // @ts-ignore
            if (authMethodServices[authName] !== undefined) {
              // @ts-ignore
              this.authMethods[authName] = authMethod;
            }
        }
    }
}

/**
 * Helper class to simplify binding the services
 */
export class ApiServiceBinder {
    constructor(private container: interfaces.Container) {
        this.container.bind(AbstractConfiguration).to(InjectableConfiguration);
    }

    /**
     * Allows you to bind a server configuration without having to import the service identifier.
     */
    public get bindServerConfiguration() {
        return this.container.bind(AbstractServerConfiguration);
    }

    /**
     * Use one of the predefined server configurations.
     *
     * To customize the server variables you can call `setVariables` on the
     * return value;
     */
    public bindServerConfigurationToPredefined(idx: number) {
        this.bindServerConfiguration.toConstantValue(servers[idx]);
        return servers[idx];
    }

    /**
     * Explicitly define the service base url
     */
    public bindServerConfigurationToURL(url: string) {
        return this.bindServerConfiguration.toConstantValue(
            new ServerConfiguration<{}>(url, {})
        );
    }

    /**
     * Allows you to bind a http library without having to import the service identifier.
     */
    public get bindHttpLibrary() {
      return this.container.bind(AbstractHttpLibrary);
    }

    /**
     * Allows you to bind a middleware without having to import the service identifier.
     *
     * You can bind multiple middlewares by calling this multiple method times.
     */
    public get bindMiddleware() {
        return this.container.bind(AbstractMiddleware);
    }

    /**
     * Allows you to bind an auth method without having to import the service identifier.
     *
     * Note: The name of the bound auth method needs to be known in the specs,
     * because the name is used to decide for which endpoints to apply the authentication.
     */
    public get bindAuthMethod() {
        return this.container.bind(AbstractAuthMethod);
    }

    /**
     * Use one of the predefined auth methods.
     *
     * Make sure that you have injected all dependencies for it.
     */
    public bindAuthMethodToPredefined(name: keyof AuthMethods) {
        return this.bindAuthMethod.to(authMethodServices[name]);
    }

    /**
     * Bind all the apis to their respective service identifiers
     *
     * If you want to only bind some of the apis, you need to do that manually.
     */
    public bindAllApiServices() {
        this.container.bind(apiServices.AbstractPromisePetApi).to(apis.PromisePetApi).inSingletonScope();
        this.container.bind(apiServices.AbstractPromiseStoreApi).to(apis.PromiseStoreApi).inSingletonScope();
        this.container.bind(apiServices.AbstractPromiseUserApi).to(apis.PromiseUserApi).inSingletonScope();
    }
}
