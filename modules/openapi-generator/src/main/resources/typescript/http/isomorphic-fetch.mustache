import {HttpLibrary, RequestContext, ResponseContext} from './http';
import * as e6p from 'es6-promise'
e6p.polyfill();
import 'isomorphic-fetch';

export class IsomorphicFetchHttpLibrary implements HttpLibrary {

    public send(request: RequestContext): Promise<ResponseContext> {
    	console.log("Request: ", request);
        let method = request.getHttpMethod().toString();
        let body = request.getBody();
        
        return fetch(request.getUrl(), {
            method: method,
            body: body as any,
            headers: request.getHeaders(),
            credentials: "same-origin"
        }).then((resp) => {
            // hack
            let headers = (resp.headers as any)._headers;
            for (let key in headers) {
                headers[key] = (headers[key] as Array<string>).join("; ");
            }

            return resp.json().then((body) => {
                return new ResponseContext(resp.status, headers, body)
            });
        });

    }
}
