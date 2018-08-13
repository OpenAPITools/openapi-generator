import {HttpLibrary} from './http/http';
import {Middleware} from './middleware';
import {IsomorphicFetchHttpLibrary} from "./http/isomorphic-fetch";
import {ServerConfiguration, servers} from './servers';

export interface ConfigurationParameters {
	baseServer?: ServerConfiguration;
    httpApi?: HttpLibrary; // override for fetch implementation
    middleware?: Middleware[]; // middleware to apply before/after fetch requests
    username?: string; // parameter for basic security
    password?: string; // parameter for basic security
    apiKey?: string | ((name: string) => string); // parameter for apiKey security
    accessToken?: string | ((name: string, scopes?: string[]) => string); // parameter for oauth2 security
}

export class Configuration {

    baseServer: ServerConfiguration;
    httpApi: HttpLibrary;
    middleware: Middleware[];
    username?: string;
    password?: string;
    apiKey?: (name: string) => string;
    accessToken?: (name: string, scopes?: string[]) => string;

    constructor(conf: ConfigurationParameters = {}) {
        this.baseServer = conf.baseServer !== undefined ? conf.baseServer : servers[0];
        this.httpApi = conf.httpApi || new IsomorphicFetchHttpLibrary(); // TODO: replace with window.fetch?
        this.middleware = conf.middleware || [];
        this.username = conf.username;
        this.password = conf.password;
        const { apiKey, accessToken } = conf;
        if (apiKey) {
            this.apiKey = typeof apiKey === 'function' ? apiKey : () => apiKey;
        }
        if (accessToken) {
            this.accessToken = typeof accessToken === 'function' ? accessToken : () => accessToken;
        }
    }
}