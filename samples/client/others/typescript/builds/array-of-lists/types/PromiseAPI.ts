import { ResponseContext, RequestContext, HttpFile, HttpInfo } from '../http/http';
import { Configuration} from '../configuration'
import { PromiseMiddleware, Middleware, PromiseMiddlewareWrapper } from "../middleware";

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
    public listWithHttpInfo(_options?: Configuration | PromiseMiddleware[]): Promise<HttpInfo<ListPaged>> {
	let observableOptions: Configuration | undefined | Middleware[]
	if (Array.isArray(_options)){
		observableOptions = _options.map(m => new PromiseMiddlewareWrapper(m))
	}else{
		observableOptions = _options
	}
        const result = this.api.listWithHttpInfo(observableOptions);
        return result.toPromise();
    }

    /**
     */
    public list(_options?: Configuration | PromiseMiddleware[]): Promise<ListPaged> {
	let observableOptions: Configuration | undefined | Middleware[]
	if (Array.isArray(_options)){
		observableOptions = _options.map(m => new PromiseMiddlewareWrapper(m))
	}else{
		observableOptions = _options
	}
        const result = this.api.list(observableOptions);
        return result.toPromise();
    }


}



