import { ResponseContext, RequestContext, HttpFile } from '../http/http';
import * as models from '../models/all';
import { Configuration} from '../configuration'

import { Cat } from '../models/Cat';
import { CatAllOf } from '../models/CatAllOf';
import { Dog } from '../models/Dog';
import { DogAllOf } from '../models/DogAllOf';
import { InlineRequest } from '../models/InlineRequest';
import { InlineRequest1 } from '../models/InlineRequest1';
import { InlineRequest2 } from '../models/InlineRequest2';
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
     * @param inlineRequest2 
     */
    public filePost(inlineRequest2?: InlineRequest2, _options?: Configuration): Promise<void> {
        const result = this.api.filePost(inlineRequest2, _options);
        return result.toPromise();
    }

    /**
     * @param inlineRequest1 
     */
    public petsFilteredPatch(inlineRequest1?: InlineRequest1, _options?: Configuration): Promise<void> {
        const result = this.api.petsFilteredPatch(inlineRequest1, _options);
        return result.toPromise();
    }

    /**
     * @param inlineRequest 
     */
    public petsPatch(inlineRequest?: InlineRequest, _options?: Configuration): Promise<void> {
        const result = this.api.petsPatch(inlineRequest, _options);
        return result.toPromise();
    }


}



