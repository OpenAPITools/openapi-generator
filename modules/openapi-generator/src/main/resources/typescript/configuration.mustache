import {HttpLibrary} from './http/http';
import {Middleware} from './middleware';
import {IsomorphicFetchHttpLibrary} from "./http/isomorphic-fetch";
import {ServerConfiguration, servers} from './servers';
import {configureAuthMethods, AuthMethods, AuthMethodsConfiguration} from './auth/auth';


export interface ConfigurationParameters {
	baseServer?: ServerConfiguration;
    httpApi?: HttpLibrary; // override for fetch implementation
    middleware?: Middleware[]; // middleware to apply before/after fetch requests
    authMethods?: AuthMethodsConfiguration
}

export class Configuration {

    baseServer: ServerConfiguration;
    httpApi: HttpLibrary;
    middleware: Middleware[];
	authMethods: AuthMethods;
	 
    constructor(conf: ConfigurationParameters = {}) {
        this.baseServer = conf.baseServer !== undefined ? conf.baseServer : servers[0];
        this.httpApi = conf.httpApi || new IsomorphicFetchHttpLibrary(); // TODO: replace with window.fetch?
        this.middleware = conf.middleware || [];
		this.authMethods = configureAuthMethods(conf.authMethods);
    }
}