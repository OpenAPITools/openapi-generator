import { ResponseContext, RequestContext, HttpFile, HttpInfo } from '../http/http';
import { Configuration, PromiseConfigurationOptions, wrapOptions } from '../configuration'
import { PromiseMiddleware, Middleware, PromiseMiddlewareWrapper } from '../middleware';

import { Cat } from '../models/Cat';
import { Dog } from '../models/Dog';
import { PetDiscriminatorResponse } from '../models/PetDiscriminatorResponse';
import { PetResponse } from '../models/PetResponse';
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
    public testDiscriminatorWithHttpInfo(_options?: PromiseConfigurationOptions): Promise<HttpInfo<PetDiscriminatorResponse>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testDiscriminatorWithHttpInfo(observableOptions);
        return result.toPromise();
    }

    /**
     */
    public testDiscriminator(_options?: PromiseConfigurationOptions): Promise<PetDiscriminatorResponse> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testDiscriminator(observableOptions);
        return result.toPromise();
    }

    /**
     */
    public testWithoutDiscriminatorWithHttpInfo(_options?: PromiseConfigurationOptions): Promise<HttpInfo<PetResponse>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testWithoutDiscriminatorWithHttpInfo(observableOptions);
        return result.toPromise();
    }

    /**
     */
    public testWithoutDiscriminator(_options?: PromiseConfigurationOptions): Promise<PetResponse> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testWithoutDiscriminator(observableOptions);
        return result.toPromise();
    }


}



