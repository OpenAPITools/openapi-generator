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

import { ObservableDefaultApi } from "./ObservableAPI";
import { DefaultApiRequestFactory, DefaultApiResponseProcessor} from "../apis/DefaultApi";

export interface DefaultApiFilePostRequest {
    /**
     * 
     * @type InlineObject
     * @memberof DefaultApifilePost
     */
    inlineObject?: InlineObject
}

export interface DefaultApiPetsFilteredPatchRequest {
    /**
     * 
     * @type PetByAge | PetByType
     * @memberof DefaultApipetsFilteredPatch
     */
    petByAgePetByType?: PetByAge | PetByType
}

export interface DefaultApiPetsPatchRequest {
    /**
     * 
     * @type Cat | Dog
     * @memberof DefaultApipetsPatch
     */
    catDog?: Cat | Dog
}

export class ObjectDefaultApi {
    private api: ObservableDefaultApi

    public constructor(configuration: Configuration, requestFactory?: DefaultApiRequestFactory, responseProcessor?: DefaultApiResponseProcessor) {
        this.api = new ObservableDefaultApi(configuration, requestFactory, responseProcessor);
    }

    /**
     * @param param the request object
     */
    public filePost(param: DefaultApiFilePostRequest = {}, options?: Configuration): Promise<void> {
        return this.api.filePost(param.inlineObject,  options).toPromise();
    }

    /**
     * @param param the request object
     */
    public petsFilteredPatch(param: DefaultApiPetsFilteredPatchRequest = {}, options?: Configuration): Promise<void> {
        return this.api.petsFilteredPatch(param.petByAgePetByType,  options).toPromise();
    }

    /**
     * @param param the request object
     */
    public petsPatch(param: DefaultApiPetsPatchRequest = {}, options?: Configuration): Promise<void> {
        return this.api.petsPatch(param.catDog,  options).toPromise();
    }

}
