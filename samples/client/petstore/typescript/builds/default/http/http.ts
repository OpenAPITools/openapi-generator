// TODO: evaluate if we can easily get rid of this library
import * as FormData from "form-data";
// typings of url-parse are incorrect...
// @ts-ignore 
import * as URLParse from "url-parse";

export * from './isomorphic-fetch';

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
export interface HttpFile {
	data: Buffer;
	name: string;
}

export interface FormEntry {
    contentDisposition: string;
    value: string | Blob;
}


export class HttpException extends Error {
    public constructor(msg: string) {
        super(msg);
    }
}

export class RequestContext {
    private headers: { [key: string]: string } = {};
    private body: string | FormData = "";
	private url: URLParse;
	
    public constructor(url: string, private httpMethod: HttpMethod) {
        this.url = URLParse(url, true);
    }
    
    public getUrl(): string {
    	return this.url.toString();
    }
    
    public setUrl(url: string) {
    	this.url = URLParse(url, true);
    }

    
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
    send(request: RequestContext): Promise<ResponseContext>;
}
