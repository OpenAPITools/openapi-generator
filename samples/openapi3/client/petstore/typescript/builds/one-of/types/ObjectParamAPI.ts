import { ResponseContext, RequestContext, HttpFile, HttpInfo } from '../http/http';
import { Configuration, ConfigurationOptions } from '../configuration'
import type { Middleware } from '../middleware';

import { Cat } from '../models/Cat';
import { Dog } from '../models/Dog';
import { PetDiscriminatorResponse } from '../models/PetDiscriminatorResponse';
import { PetResponse } from '../models/PetResponse';

import { ObservableDefaultApi } from "./ObservableAPI";
import { DefaultApiRequestFactory, DefaultApiResponseProcessor} from "../apis/DefaultApi";

export interface DefaultApiTestDiscriminatorRequest {
}

export interface DefaultApiTestWithoutDiscriminatorRequest {
}

export class ObjectDefaultApi {
    private api: ObservableDefaultApi

    public constructor(configuration: Configuration, requestFactory?: DefaultApiRequestFactory, responseProcessor?: DefaultApiResponseProcessor) {
        this.api = new ObservableDefaultApi(configuration, requestFactory, responseProcessor);
    }

    /**
     * @param param the request object
     */
    public testDiscriminatorWithHttpInfo(param: DefaultApiTestDiscriminatorRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<PetDiscriminatorResponse>> {
        return this.api.testDiscriminatorWithHttpInfo( options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testDiscriminator(param: DefaultApiTestDiscriminatorRequest = {}, options?: ConfigurationOptions): Promise<PetDiscriminatorResponse> {
        return this.api.testDiscriminator( options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testWithoutDiscriminatorWithHttpInfo(param: DefaultApiTestWithoutDiscriminatorRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<PetResponse>> {
        return this.api.testWithoutDiscriminatorWithHttpInfo( options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testWithoutDiscriminator(param: DefaultApiTestWithoutDiscriminatorRequest = {}, options?: ConfigurationOptions): Promise<PetResponse> {
        return this.api.testWithoutDiscriminator( options).toPromise();
    }

}
