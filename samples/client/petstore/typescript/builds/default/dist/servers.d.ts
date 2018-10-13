import { RequestContext, HttpMethod } from './http/http';
export declare class ServerConfiguration {
    private url;
    constructor(url: string);
    makeRequestContext(endpoint: string, httpMethod: HttpMethod): RequestContext;
}
export declare const servers: ServerConfiguration[];
