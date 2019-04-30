import {HttpLibrary} from './http/http';
import {Middleware, PromiseMiddleware, PromiseMiddlewareWrapper} from './middleware';
import {IsomorphicFetchHttpLibrary} from "./http/isomorphic-fetch";
import {ServerConfiguration, server1} from './servers';
import {configureAuthMethods, AuthMethods, AuthMethodsConfiguration} from './auth/auth';

/**
 * Inetrface with which a configuration object can be configured.
 *
 */
export interface ConfigurationParameters {
	/**
	 * Default server to use
	 */
	baseServer?: ServerConfiguration<any>;
	/**
	 * HTTP library to use e.g. IsomorphicFetch
	 */
    httpApi?: HttpLibrary;
    /**
     * The middlewares which will be applied to requests and responses
     */
    middleware?: Middleware[]; // middleware to apply before/after fetch requests
    /**
     * configures all middlewares using the promise api instead of observables (which Middleware uses)
     */
    promiseMiddleware?: PromiseMiddleware[];
    /**
     * Configuration for the available authentication methods
     */
    authMethods?: AuthMethodsConfiguration
}

export class Configuration {

    baseServer: ServerConfiguration<any>;
    httpApi: HttpLibrary;
    middleware: Middleware[];
	authMethods: AuthMethods;
	 
	/**
	 * Creates a new configuration object based on the given configuration.
	 * If a property is not included in conf, a default is used:
	 * 		- baseServer: server1
	 * 		- httpApi: IsomorphicFetchHttpLibrary
	 *		- middleware: []
	 *		- promiseMiddleware: []
	 *		- authMethods: {}
	 * @param conf particial configuration 
	 */
    constructor(conf: ConfigurationParameters = {}) {
        this.baseServer = conf.baseServer !== undefined ? conf.baseServer : server1;
        this.httpApi = conf.httpApi || new IsomorphicFetchHttpLibrary(); // TODO: replace with window.fetch if available?
        this.middleware = conf.middleware || [];
		this.authMethods = configureAuthMethods(conf.authMethods);
        if (conf.promiseMiddleware) {
            conf.promiseMiddleware.forEach(m => this.middleware.push(new PromiseMiddlewareWrapper(m)));
        }
    }
}