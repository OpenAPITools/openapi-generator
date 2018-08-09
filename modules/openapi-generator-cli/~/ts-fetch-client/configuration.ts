import {HttpLibrary} from './http/http';
import {Middleware} from './middleware';

export interface ConfigurationParameters {
    basePath?: string; // override base path
    httpApi?: HttpLibrary; // override for fetch implementation
    middleware?: Middleware[]; // middleware to apply before/after fetch requests
    username?: string; // parameter for basic security
    password?: string; // parameter for basic security
    apiKey?: string | ((name: string) => string); // parameter for apiKey security
    accessToken?: string | ((name: string, scopes?: string[]) => string); // parameter for oauth2 security
}

export class Configuration {

    basePath: string;
    httpApi: HttpLibrary;
    middleware: Middleware[];
    username?: string;
    password?: string;
    apiKey?: (name: string) => string;
    accessToken?: (name: string, scopes?: string[]) => string;

    constructor(conf: ConfigurationParameters = {}) {
        this.basePath = conf.basePath !== undefined ? conf.basePath : BASE_PATH;
        this.fetchApi = conf.fetchApi || window.fetch.bind(window);
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