declare var fetch: any;

import {HttpLibrary, RequestContext, ResponseContext} from './http';
import * as e6p from 'es6-promise'
import { from, Observable } from '../rxjsStub';
e6p.polyfill();
import 'isomorphic-fetch';

export class IsomorphicFetchHttpLibrary implements HttpLibrary {

    public send(request: RequestContext): Observable<ResponseContext> {
        let method = request.getHttpMethod().toString();
        let body = request.getBody();
        
        const resultPromise = fetch(request.getUrl(), {
            method: method,
            body: body as any,
            headers: request.getHeaders(),
            credentials: "same-origin"
        }).then((resp: any) => {
            // hack
            let headers = (resp.headers as any)._headers;
            for (let key in headers) {
                headers[key] = (headers[key] as Array<string>).join("; ");
            }

            return resp.text().then((body: string) => {
                return new ResponseContext(resp.status, headers, body)
            });
        });
        
        return from<Promise<ResponseContext>>(resultPromise);

    }
}
