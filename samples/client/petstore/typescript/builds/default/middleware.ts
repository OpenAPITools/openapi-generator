import {RequestContext, ResponseContext} from './http/http';
import { Observable, from } from 'rxjs';

export interface Middleware {
    pre(context: RequestContext): Observable<RequestContext>;
    post(context: ResponseContext): Observable<ResponseContext>;
}

export class PromiseMiddlewareWrapper implements Middleware {

    public constructor(private middleware: PromiseMiddleware) {

    }

    pre(context: RequestContext): Observable<RequestContext> {
        return from(this.middleware.pre(context));
    }
    
    post(context: ResponseContext): Observable<ResponseContext> {
        return from(this.middleware.post(context));
    }

}

export interface PromiseMiddleware {
    pre(context: RequestContext): Observable<RequestContext>;
    post(context: ResponseContext): Observable<ResponseContext>;
}