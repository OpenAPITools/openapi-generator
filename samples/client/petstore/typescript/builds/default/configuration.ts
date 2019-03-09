import {HttpLibrary} from './http/http';
import {Middleware, PromiseMiddleware, PromiseMiddlewareWrapper} from './middleware';
import {IsomorphicFetchHttpLibrary} from "./http/isomorphic-fetch";
import {ServerConfiguration, server1} from './servers';
import {configureAuthMethods, AuthMethods, AuthMethodsConfiguration} from './auth/auth';


export interface ConfigurationParameters {
	baseServer?: ServerConfiguration<any>;
    httpApi?: HttpLibrary; // override for fetch implementation
    middleware?: Middleware[]; // middleware to apply before/after fetch requests
    promiseMiddleware?: PromiseMiddleware[];
    authMethods?: AuthMethodsConfiguration
}

export class Configuration {

    baseServer: ServerConfiguration<any>;
    httpApi: HttpLibrary;
    middleware: Middleware[];
	authMethods: AuthMethods;
	 
    constructor(conf: ConfigurationParameters = {}) {
        this.baseServer = conf.baseServer !== undefined ? conf.baseServer : server1;
        this.httpApi = conf.httpApi || new IsomorphicFetchHttpLibrary(); // TODO: replace with window.fetch?
        this.middleware = conf.middleware || [];
		this.authMethods = configureAuthMethods(conf.authMethods);
        if (conf.promiseMiddleware) {
            conf.promiseMiddleware.forEach(m => this.middleware.push(new PromiseMiddlewareWrapper(m)));
        }
    }
}