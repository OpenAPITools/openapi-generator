// TODO: evaluate if we can easily get rid of this library
import * as FormData from "form-data";
// typings of url-parse are incorrect...
// @ts-ignore 
import * as URLParse from "url-parse";
import { Observable } from '../rxjsStub';

export * from './isomorphic-fetch';

/**
 * Represents a HTTP Method.
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
 * Represents a http file which will be uploaded to a server.
 */
export interface HttpFile {
	data: Buffer;
	name: string;
}


export class HttpException extends Error {
    public constructor(msg: string) {
        super(msg);
    }
}

/**
 * Represents a HTTP request context 
 *
 */
export class RequestContext {
    private headers: { [key: string]: string } = {};
    private body: string | FormData = "";
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
     * Setting a body on a HTTP GET request is disallowed under HTTP-Spec 1.1. Section
     * 4.3 and this method throws an HttpException accordingly.
     *
     * @param body the body of the request
     */
    public setBody(body: string | FormData) {
        // HTTP-Spec 1.1 Section 4.3
        if (this.httpMethod === HttpMethod.GET) {
            throw new HttpException("Body should not be included in GET-Requests!");
        }
        
        // TODO: other http methods
        
        // post is fine either formData or string
        this.body = body;
        
    }
    
    public getHttpMethod(): HttpMethod {
    	return this.httpMethod;
    }
    
    public getHeaders(): { [key: string]: string } {
    	return this.headers;
    }
    
    public getBody(): string | FormData { 
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