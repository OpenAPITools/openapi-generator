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

import { ObservableDefaultApi } from "./ObservableAPI";
import { DefaultApiRequestFactory, DefaultApiResponseProcessor} from "../apis/DefaultApi";

export interface DefaultApiFilePostRequest {
    /**
     * 
     * @type InlineRequest2
     * @memberof DefaultApifilePost
     */
    inlineRequest2?: InlineRequest2
}

export interface DefaultApiPetsFilteredPatchRequest {
    /**
     * 
     * @type InlineRequest1
     * @memberof DefaultApipetsFilteredPatch
     */
    inlineRequest1?: InlineRequest1
}

export interface DefaultApiPetsPatchRequest {
    /**
     * 
     * @type InlineRequest
     * @memberof DefaultApipetsPatch
     */
    inlineRequest?: InlineRequest
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
        return this.api.filePost(param.inlineRequest2,  options).toPromise();
    }

    /**
     * @param param the request object
     */
    public petsFilteredPatch(param: DefaultApiPetsFilteredPatchRequest = {}, options?: Configuration): Promise<void> {
        return this.api.petsFilteredPatch(param.inlineRequest1,  options).toPromise();
    }

    /**
     * @param param the request object
     */
    public petsPatch(param: DefaultApiPetsPatchRequest = {}, options?: Configuration): Promise<void> {
        return this.api.petsPatch(param.inlineRequest,  options).toPromise();
    }

}
