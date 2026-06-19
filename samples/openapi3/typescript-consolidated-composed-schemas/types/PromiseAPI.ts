import { ResponseContext, RequestContext, HttpFile, HttpInfo } from '../http/http';
import { Configuration, PromiseConfigurationOptions, wrapOptions } from '../configuration'
import { PromiseMiddleware, Middleware, PromiseMiddlewareWrapper } from '../middleware';

import { Cat } from '../models/Cat';
import { Dog } from '../models/Dog';
import { FilePostRequest } from '../models/FilePostRequest';
import { PetByAge } from '../models/PetByAge';
import { PetByType } from '../models/PetByType';
import { PetsFilteredPatchRequest } from '../models/PetsFilteredPatchRequest';
import { PetsPatchRequest } from '../models/PetsPatchRequest';
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
     * @param [filePostRequest]
     */
    public filePostWithHttpInfo(filePostRequest?: FilePostRequest, _options?: PromiseConfigurationOptions): Promise<HttpInfo<void>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.filePostWithHttpInfo(filePostRequest, observableOptions);
        return result.toPromise();
    }

    /**
     * @param [filePostRequest]
     */
    public filePost(filePostRequest?: FilePostRequest, _options?: PromiseConfigurationOptions): Promise<void> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.filePost(filePostRequest, observableOptions);
        return result.toPromise();
    }

    /**
     * @param [petsFilteredPatchRequest]
     */
    public petsFilteredPatchWithHttpInfo(petsFilteredPatchRequest?: PetsFilteredPatchRequest, _options?: PromiseConfigurationOptions): Promise<HttpInfo<void>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.petsFilteredPatchWithHttpInfo(petsFilteredPatchRequest, observableOptions);
        return result.toPromise();
    }

    /**
     * @param [petsFilteredPatchRequest]
     */
    public petsFilteredPatch(petsFilteredPatchRequest?: PetsFilteredPatchRequest, _options?: PromiseConfigurationOptions): Promise<void> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.petsFilteredPatch(petsFilteredPatchRequest, observableOptions);
        return result.toPromise();
    }

    /**
     * @param [petsPatchRequest]
     */
    public petsPatchWithHttpInfo(petsPatchRequest?: PetsPatchRequest, _options?: PromiseConfigurationOptions): Promise<HttpInfo<void>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.petsPatchWithHttpInfo(petsPatchRequest, observableOptions);
        return result.toPromise();
    }

    /**
     * @param [petsPatchRequest]
     */
    public petsPatch(petsPatchRequest?: PetsPatchRequest, _options?: PromiseConfigurationOptions): Promise<void> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.petsPatch(petsPatchRequest, observableOptions);
        return result.toPromise();
    }


}



