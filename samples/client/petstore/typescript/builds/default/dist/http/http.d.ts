import * as FormData from "form-data";
export declare enum HttpMethod {
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
export interface FormEntry {
    contentDisposition: string;
    value: string | Blob;
}
export declare class HttpException extends Error {
    constructor(msg: string);
}
export declare class RequestContext {
    private httpMethod;
    private headers;
    private body;
    private url;
    constructor(url: string, httpMethod: HttpMethod);
    getUrl(): string;
    setUrl(url: string): void;
    setBody(body: string | FormData): void;
    getHttpMethod(): HttpMethod;
    getHeaders(): {
        [key: string]: string;
    };
    getBody(): string | FormData;
    setQueryParam(name: string, value: string): void;
    addCookie(name: string, value: string): void;
    setHeaderParam(key: string, value: string): void;
}
export declare class ResponseContext {
    httpStatusCode: number;
    headers: {
        [key: string]: string;
    };
    body: string;
    constructor(httpStatusCode: number, headers: {
        [key: string]: string;
    }, body: string);
}
export interface HttpLibrary {
    send(request: RequestContext): Promise<ResponseContext>;
}
