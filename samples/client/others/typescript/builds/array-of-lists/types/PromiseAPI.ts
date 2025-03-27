import { ResponseContext, RequestContext, HttpFile, HttpInfo } from '../http/http';
import { Configuration, ConfigurationOptions, PromiseConfigurationOptions, wrapOptions } from '../configuration'
import { PromiseMiddleware, Middleware, PromiseMiddlewareWrapper } from '../middleware';

import { List } from '../models/List';
import { ListPaged } from '../models/ListPaged';
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
    public listWithHttpInfo(_options?: PromiseConfigurationOptions): Promise<HttpInfo<ListPaged>> {
        let observableOptions: undefined | ConfigurationOptions = wrapOptions(_options);
        const result = this.api.listWithHttpInfo(observableOptions);
        return result.toPromise();
    }

    /**
     */
    public list(_options?: PromiseConfigurationOptions): Promise<ListPaged> {
        let observableOptions: undefined | ConfigurationOptions = wrapOptions(_options);
        const result = this.api.list(observableOptions);
        return result.toPromise();
    }


}



