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
     * @param petByAgePetByType 
     */
    public petsFilteredPatch(petByAgePetByType?: PetByAge | PetByType, _options?: Configuration): Promise<void> {
        const result = this.api.petsFilteredPatch(petByAgePetByType, _options);
        return result.toPromise();
    }

    /**
     * @param catDog 
     */
    public petsPatch(catDog?: Cat | Dog, _options?: Configuration): Promise<void> {
        const result = this.api.petsPatch(catDog, _options);
        return result.toPromise();
    }


}



