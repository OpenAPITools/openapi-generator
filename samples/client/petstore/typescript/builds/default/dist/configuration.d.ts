import { HttpLibrary } from './http/http';
import { Middleware } from './middleware';
import { ServerConfiguration } from './servers';
import { AuthMethods, AuthMethodsConfiguration } from './auth/auth';
export interface ConfigurationParameters {
    baseServer?: ServerConfiguration;
    httpApi?: HttpLibrary;
    middleware?: Middleware[];
    authMethods?: AuthMethodsConfiguration;
}
export declare class Configuration {
    baseServer: ServerConfiguration;
    httpApi: HttpLibrary;
    middleware: Middleware[];
    authMethods: AuthMethods;
    constructor(conf?: ConfigurationParameters);
}
