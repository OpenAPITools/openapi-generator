import IHttpClient from "./IHttpClient";
import { Observable } from "rxjs/Observable";
import "whatwg-fetch";
import HttpResponse from "./HttpResponse";
import {injectable} from "inversify";
import "rxjs/add/observable/fromPromise";
import { Headers } from "./Headers";

@injectable()
class HttpClient implements IHttpClient {

    get(url:string, headers?: Headers):Observable<HttpResponse> {
        return this.performNetworkCall(url, "get", undefined, headers);
    }

    post(url: string, body: {}|FormData, headers?: Headers): Observable<HttpResponse> {
        return this.performNetworkCall(url, "post", this.getJsonBody(body), this.addJsonHeaders(headers));
    }

    put(url: string, body: {}, headers?: Headers): Observable<HttpResponse> {
        return this.performNetworkCall(url, "put", this.getJsonBody(body), this.addJsonHeaders(headers));
    }

    delete(url: string, headers?: Headers): Observable<HttpResponse> {
        return this.performNetworkCall(url, "delete", undefined, headers);
    }

    private getJsonBody(body: {}|FormData) {
        return !(body instanceof FormData) ? JSON.stringify(body) : body;
    }

    private addJsonHeaders(headers: Headers) {
        return Object.assign({}, {
            "Accept": "application/json",
            "Content-Type": "application/json"
        }, headers);
    };

    private performNetworkCall(url: string, method: string, body?: any, headers?: Headers): Observable<HttpResponse> {
        let promise = window.fetch(url, {
            method: method,
            body: body,
            headers: <any>headers
        }).then(response => {
            let headers: Headers = {};
            response.headers.forEach((value, name) => {
                headers[name.toString().toLowerCase()] = value;
            });
            return response.text().then(text => {
                let contentType = headers["content-type"] || "";
                let payload = contentType.match("application/json") ? JSON.parse(text) : text;
                let httpResponse = new HttpResponse(payload, response.status, headers);

                if (response.status >= 400)
                    throw httpResponse;
                return httpResponse;
            });
        });
        return Observable.fromPromise(promise);
    }
}

export default HttpClient