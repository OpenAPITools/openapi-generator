import { ResponseContext, RequestContext, HttpFile, HttpInfo } from '../http/http';
import { Configuration} from '../configuration'
import type { Middleware } from "../middleware";
import { Observable, of, from } from '../rxjsStub';
import {mergeMap, map} from  '../rxjsStub';
import { Response } from '../models/Response';

import { DefaultApiRequestFactory, DefaultApiResponseProcessor} from "../apis/DefaultApi";
export class ObservableDefaultApi {
    private requestFactory: DefaultApiRequestFactory;
    private responseProcessor: DefaultApiResponseProcessor;
    private configuration: Configuration;

    public constructor(
        configuration: Configuration,
        requestFactory?: DefaultApiRequestFactory,
        responseProcessor?: DefaultApiResponseProcessor
    ) {
        this.configuration = configuration;
        this.requestFactory = requestFactory || new DefaultApiRequestFactory(configuration);
        this.responseProcessor = responseProcessor || new DefaultApiResponseProcessor();
    }

    /**
     */
    public uniqueItemsWithHttpInfo(_options?: Configuration | Middleware[]): Observable<HttpInfo<Response>> {
    	let configuration = undefined
	let calltimeMiddleware: Middleware[] = []
	if (Array.isArray(_options)){
	    // call-time middleware provided
	    calltimeMiddleware = _options
	}else{
	    configuration = _options
	}
        const requestContextPromise = this.requestFactory.uniqueItems(_options);

        // build promise chain
	let allMiddleware = this.configuration.middleware.concat(calltimeMiddleware)
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of allMiddleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => this.configuration.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of this.configuration.middleware) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.uniqueItemsWithHttpInfo(rsp)));
            }));
    }

    /**
     */
    public uniqueItems(_options?: Configuration | Middleware[]): Observable<Response> {
        return this.uniqueItemsWithHttpInfo(_options).pipe(map((apiResponse: HttpInfo<Response>) => apiResponse.data));
    }

}
