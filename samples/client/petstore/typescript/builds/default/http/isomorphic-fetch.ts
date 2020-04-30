declare var fetch: any;

import {HttpLibrary, RequestContext, ResponseContext} from './http';
import 'es6-promise/auto';
import { from, Observable } from '../rxjsStub';
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
            const headers: { [name: string]: string } = {};
            resp.headers.forEach((value: string, name: string) => {
              headers[name] = value;
            });

            return resp.text().then((body: string) => {
                return new ResponseContext(resp.status, headers, body)
            });
        });

        return from<Promise<ResponseContext>>(resultPromise);

    }
}
