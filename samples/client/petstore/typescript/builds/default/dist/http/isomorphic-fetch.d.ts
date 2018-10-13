import { HttpLibrary, RequestContext, ResponseContext } from './http';
import 'isomorphic-fetch';
export declare class IsomorphicFetchHttpLibrary implements HttpLibrary {
    send(request: RequestContext): Promise<ResponseContext>;
}
