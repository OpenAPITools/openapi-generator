import type { PromiseHttpLibrary, Headers, HttpMethod, RequestContext, ResponseContext } from "../http/http";
import type { PromiseMiddleware } from "../middleware";
import type { BaseServerConfiguration } from "../servers";

export abstract class AbstractHttpLibrary implements PromiseHttpLibrary {
    public abstract send(request: RequestContext): Promise<ResponseContext>;
};

export abstract class AbstractMiddleware implements PromiseMiddleware {
    public abstract pre(context: RequestContext): Promise<RequestContext>;
    public abstract post(context: ResponseContext): Promise<ResponseContext>;
}

export abstract class AbstractServerConfiguration implements BaseServerConfiguration {
    public abstract getHeaders(): Headers
    public abstract makeRequestContext(endpoint: string, httpMethod: HttpMethod): RequestContext;
    public abstract addHeaders(headers: Headers): any;
    public abstract setHeaderParam(key: string, value: string): void
};
