import type { Configuration } from "../configuration";
import type { HttpFile, RequestContext, ResponseContext, HttpInfo } from "../http/http";

import { ApiResponse } from "../models/ApiResponse";
import { Pet } from "../models/Pet";

export abstract class AbstractPetApiRequestFactory {
    public abstract addPet(pet: Pet, options?: Configuration): Promise<RequestContext>;

    public abstract deletePet(petId: number, apiKey?: string, options?: Configuration): Promise<RequestContext>;

    public abstract findPetsByStatus(status: Array<'available' | 'pending' | 'sold'>, options?: Configuration): Promise<RequestContext>;

    public abstract findPetsByTags(tags: Array<string>, options?: Configuration): Promise<RequestContext>;

    public abstract getPetById(petId: number, options?: Configuration): Promise<RequestContext>;

    public abstract updatePet(pet: Pet, options?: Configuration): Promise<RequestContext>;

    public abstract updatePetWithForm(petId: number, name?: string, status?: string, options?: Configuration): Promise<RequestContext>;

    public abstract uploadFile(petId: number, additionalMetadata?: string, file?: HttpFile, options?: Configuration): Promise<RequestContext>;

}


export abstract class AbstractPetApiResponseProcessor {
     public abstract addPetWithHttpInfo(response: ResponseContext): Promise<HttpInfo<Pet >>;

     public abstract deletePetWithHttpInfo(response: ResponseContext): Promise<HttpInfo< void>>;

     public abstract findPetsByStatusWithHttpInfo(response: ResponseContext): Promise<HttpInfo<Array<Pet> >>;

     public abstract findPetsByTagsWithHttpInfo(response: ResponseContext): Promise<HttpInfo<Array<Pet> >>;

     public abstract getPetByIdWithHttpInfo(response: ResponseContext): Promise<HttpInfo<Pet >>;

     public abstract updatePetWithHttpInfo(response: ResponseContext): Promise<HttpInfo<Pet >>;

     public abstract updatePetWithFormWithHttpInfo(response: ResponseContext): Promise<HttpInfo< void>>;

     public abstract uploadFileWithHttpInfo(response: ResponseContext): Promise<HttpInfo<ApiResponse >>;

}
