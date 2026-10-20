import { ResponseContext, RequestContext, HttpFile, HttpInfo } from '../http/http';
import { Configuration, PromiseConfigurationOptions, wrapOptions } from '../configuration'
import { PromiseMiddleware, Middleware, PromiseMiddlewareWrapper } from '../middleware';

import { Response } from '../models/Response';
import { ObservableDefaultApi } from './ObservableAPI';

import { DefaultApiRequestFactory, DefaultApiResponseProcessor} from "../apis/DefaultApi";
export class PromiseDefaultApi {
    private api: ObservableDefaultApi

    public constructor(
        configuration: Configuration,
        requestFactory?: DefaultApiRequestFactory,
        responseProcessor?: DefaultApiResponseProcessor
    ) {
        this.api = new ObservableDefaultApi(configuration, requestFactory, responseProcessor);
    }

    /**
     */
    public uniqueItemsWithHttpInfo(_options?: PromiseConfigurationOptions): Promise<HttpInfo<Response>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.uniqueItemsWithHttpInfo(observableOptions);
        return result.toPromise();
    }

    /**
     */
    public uniqueItems(_options?: PromiseConfigurationOptions): Promise<Response> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.uniqueItems(observableOptions);
        return result.toPromise();
    }


}



