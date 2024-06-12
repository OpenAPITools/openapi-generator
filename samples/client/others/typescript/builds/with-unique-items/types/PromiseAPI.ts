import { ResponseContext, RequestContext, HttpFile, HttpInfo } from '../http/http';
import { Configuration} from '../configuration'

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
    public uniqueItemsWithHttpInfo(_options?: Configuration): Promise<HttpInfo<Response>> {
        const result = this.api.uniqueItemsWithHttpInfo(_options);
        return result.toPromise();
    }

    /**
     */
    public uniqueItems(_options?: Configuration): Promise<Response> {
        const result = this.api.uniqueItems(_options);
        return result.toPromise();
    }


}



