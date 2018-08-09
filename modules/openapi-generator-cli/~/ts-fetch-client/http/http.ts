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

export interface FormEntry {
    contentType: string;
    value: string | Blob;
}

export type FormData = { [key: string]: FormEntry };


export class RequestContext {
    public headers: { [key: string]: string } = {};
    public body: string | FormData = "";

    public constructor(public url: string, public httpMethod: HttpMethod) {

    }

    public addCookie(name: string, value: string): void {
        if (!this.headers["Cookie"]) {
            this.headers["Cookie"] = "";
        }
        this.headers["Cookie"] += name + "=" + value + "; ";
    }

    public setHeader(key: string, value: string): void  { 
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