import { ResponseContext, RequestContext, HttpFile, HttpInfo } from '../http/http.ts';
import { Configuration} from '../configuration.ts'
import { Observable, of, from } from '../rxjsStub.ts';
import {mergeMap, map} from  '../rxjsStub.ts';
import { ChatMessage } from '../models/ChatMessage.ts';
import { Email } from '../models/Email.ts';
import { Interaction } from '../models/Interaction.ts';

import { DefaultApiRequestFactory, DefaultApiResponseProcessor} from "../apis/DefaultApi.ts";
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
     * Optional extended description in Markdown.
     * Returns all interactions.
     * @param parameterTypeMapping 
     */
    public interactionsGetWithHttpInfo(parameterTypeMapping?: string, _options?: Configuration): Observable<HttpInfo<Interaction>> {
        const requestContextPromise = this.requestFactory.interactionsGet(parameterTypeMapping, _options);

        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (let middleware of this.configuration.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => this.configuration.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (let middleware of this.configuration.middleware) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.interactionsGetWithHttpInfo(rsp)));
            }));
    }

    /**
     * Optional extended description in Markdown.
     * Returns all interactions.
     * @param parameterTypeMapping 
     */
    public interactionsGet(parameterTypeMapping?: string, _options?: Configuration): Observable<Interaction> {
        return this.interactionsGetWithHttpInfo(parameterTypeMapping, _options).pipe(map((apiResponse: HttpInfo<Interaction>) => apiResponse.data));
    }

}
