// typings of url-parse are incorrect...
// @ts-ignore 
import * as URLParse from "url-parse";
import { Observable } from '../rxjsStub';

export * from './jquery';

/**
 * Represents an HTTP method.
 */
export enum HttpMethod {
    GET = "GET",
    HEAD = "HEAD",
    POST = "POST",
    PUT = "PUT",
    DELETE = "DELETE",
    CONNECT = "CONNECT",
    OPTIONS = "OPTIONS",
    TRACE = "TRACE",
    PATCH = "PATCH"
}

/**
 * Represents an HTTP file which will be transferred from or to a server.
 */
export type HttpFile = Blob & { readonly name: string };


export class HttpException extends Error {
    public constructor(msg: string) {
        super(msg);
    }
}

/**
 * Represents the body of an outgoing HTTP request.
 */
export type RequestBody = undefined | string | FormData;

/**
 * Represents an HTTP request context
 */
export class RequestContext {
    private headers: { [key: string]: string } = {};
    private body: RequestBody = undefined;
    private url: URLParse;

	/**
	 * Creates the request context using a http method and request resource url
	 *
	 * @param url url of the requested resource
	 * @param httpMethod http method
	 */
    public constructor(url: string, private httpMethod: HttpMethod) {
        this.url = URLParse(url, true);
    }
    
    /*
     * Returns the url set in the constructor including the query string
     *
     */
    public getUrl(): string {
    	return this.url.toString();
    }
    
    /**
     * Replaces the url set in the constructor with this url.
     *
     */
    public setUrl(url: string) {
    	this.url = URLParse(url, true);
    }

    /**
     * Sets the body of the http request either as a string or FormData
     *
     * Note that setting a body on a HTTP GET, HEAD, DELETE, CONNECT or TRACE
     * request is discouraged.
     * https://httpwg.org/http-core/draft-ietf-httpbis-semantics-latest.html#rfc.section.7.3.1
     *
     * @param body the body of the request
     */
    public setBody(body: RequestBody) {
        this.body = body;
    }

    public getHttpMethod(): HttpMethod {
    	return this.httpMethod;
    }
    
    public getHeaders(): { [key: string]: string } {
    	return this.headers;
    }

    public getBody(): RequestBody {
        return this.body;
    }

	public setQueryParam(name: string, value: string) {
        let queryObj = this.url.query;
        queryObj[name] = value;
        this.url.set("query", queryObj);
    }

	/**
	 *	Sets a cookie with the name and value. NO check  for duplicate cookies is performed
	 *
	 */
    public addCookie(name: string, value: string): void {
        if (!this.headers["Cookie"]) {
            this.headers["Cookie"] = "";
        }
        this.headers["Cookie"] += name + "=" + value + "; ";
    }

    public setHeaderParam(key: string, value: string): void  { 
        this.headers[key] = value;
    }
}

export class ResponseContext {

    public constructor(public httpStatusCode: number, 
        public headers: { [key: string]: string }, public body: string) {
    }
    
}

export interface HttpLibrary {
    send(request: RequestContext): Observable<ResponseContext>;
}