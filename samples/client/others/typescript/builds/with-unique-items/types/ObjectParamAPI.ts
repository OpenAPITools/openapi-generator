import { ResponseContext, RequestContext, HttpFile, HttpInfo } from '../http/http';
import { Configuration} from '../configuration'

import { Response } from '../models/Response';

import { ObservableDefaultApi } from "./ObservableAPI";
import { DefaultApiRequestFactory, DefaultApiResponseProcessor} from "../apis/DefaultApi";

export interface DefaultApiUniqueItemsRequest {
}

export class ObjectDefaultApi {
    private api: ObservableDefaultApi

    public constructor(configuration: Configuration, requestFactory?: DefaultApiRequestFactory, responseProcessor?: DefaultApiResponseProcessor) {
        this.api = new ObservableDefaultApi(configuration, requestFactory, responseProcessor);
    }

    /**
     * @param param the request object
     */
    public uniqueItemsWithHttpInfo(param: DefaultApiUniqueItemsRequest = {}, options?: Configuration): Promise<HttpInfo<Response>> {
        return this.api.uniqueItemsWithHttpInfo( options).toPromise();
    }

    /**
     * @param param the request object
     */
    public uniqueItems(param: DefaultApiUniqueItemsRequest = {}, options?: Configuration): Promise<Response> {
        return this.api.uniqueItems( options).toPromise();
    }

}
