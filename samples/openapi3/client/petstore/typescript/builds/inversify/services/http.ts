import type { HttpLibrary, HttpMethod, RequestContext, ResponseContext } from "../http/http";
import type { Observable } from "../rxjsStub";
import type { BaseServerConfiguration } from "../servers";

export abstract class AbstractHttpLibrary implements HttpLibrary {
    public abstract send(request: RequestContext): Observable<ResponseContext>;
};

export abstract class AbstractMiddleware {
    public abstract pre(context: RequestContext): Observable<RequestContext>;
    public abstract post(context: ResponseContext): Observable<ResponseContext>;
}

export abstract class AbstractServerConfiguration implements BaseServerConfiguration {
    public abstract makeRequestContext(endpoint: string, httpMethod: HttpMethod): RequestContext;
};

