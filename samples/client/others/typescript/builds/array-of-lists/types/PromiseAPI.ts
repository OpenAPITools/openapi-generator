import { ResponseContext, RequestContext, HttpFile, HttpInfo } from '../http/http';
import { Configuration} from '../configuration'

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
    public listWithHttpInfo(_options?: Configuration): Promise<HttpInfo<ListPaged>> {
        const result = this.api.listWithHttpInfo(_options);
        return result.toPromise();
    }

    /**
     */
    public list(_options?: Configuration): Promise<ListPaged> {
        const result = this.api.list(_options);
        return result.toPromise();
    }


}



