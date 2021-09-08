import { ResponseContext, RequestContext, HttpFile } from '../http/http';
import * as models from '../models/all';
import { Configuration} from '../configuration'

import { Cat } from '../models/Cat';
import { CatAllOf } from '../models/CatAllOf';
import { Dog } from '../models/Dog';
import { DogAllOf } from '../models/DogAllOf';
import { InlineObject } from '../models/InlineObject';
import { PetByAge } from '../models/PetByAge';
import { PetByType } from '../models/PetByType';
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
     * @param inlineObject 
     */
    public filePost(inlineObject?: InlineObject, _options?: Configuration): Promise<void> {
        const result = this.api.filePost(inlineObject, _options);
        return result.toPromise();
    }

    /**
     * @param UNKNOWN_BASE_TYPE 
     */
    public petsFilteredPatch(UNKNOWN_BASE_TYPE?: UNKNOWN_BASE_TYPE, _options?: Configuration): Promise<void> {
        const result = this.api.petsFilteredPatch(UNKNOWN_BASE_TYPE, _options);
        return result.toPromise();
    }

    /**
     * @param UNKNOWN_BASE_TYPE 
     */
    public petsPatch(UNKNOWN_BASE_TYPE?: UNKNOWN_BASE_TYPE, _options?: Configuration): Promise<void> {
        const result = this.api.petsPatch(UNKNOWN_BASE_TYPE, _options);
        return result.toPromise();
    }


}



