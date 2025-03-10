import { ResponseContext, RequestContext, HttpFile, HttpInfo } from '../http/http';
import { Configuration, ConfigurationOptions } from '../configuration'
import type { Middleware } from '../middleware';

import { List } from '../models/List';
import { ListPaged } from '../models/ListPaged';

import { ObservableDefaultApi } from "./ObservableAPI";
import { DefaultApiRequestFactory, DefaultApiResponseProcessor} from "../apis/DefaultApi";

export interface DefaultApiListRequest {
}

export class ObjectDefaultApi {
    private api: ObservableDefaultApi

    public constructor(configuration: Configuration, requestFactory?: DefaultApiRequestFactory, responseProcessor?: DefaultApiResponseProcessor) {
        this.api = new ObservableDefaultApi(configuration, requestFactory, responseProcessor);
    }

    /**
     * @param param the request object
     */
    public listWithHttpInfo(param: DefaultApiListRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<ListPaged>> {
        return this.api.listWithHttpInfo( options).toPromise();
    }

    /**
     * @param param the request object
     */
    public list(param: DefaultApiListRequest = {}, options?: ConfigurationOptions): Promise<ListPaged> {
        return this.api.list( options).toPromise();
    }

}
