import type { Configuration } from "../configuration";
import type { HttpFile, RequestContext, ResponseContext } from "../http/http";

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
     public abstract addPet(response: ResponseContext): Promise<Pet >;

     public abstract deletePet(response: ResponseContext): Promise< void>;

     public abstract findPetsByStatus(response: ResponseContext): Promise<Array<Pet> >;

     public abstract findPetsByTags(response: ResponseContext): Promise<Array<Pet> >;

     public abstract getPetById(response: ResponseContext): Promise<Pet >;

     public abstract updatePet(response: ResponseContext): Promise<Pet >;

     public abstract updatePetWithForm(response: ResponseContext): Promise< void>;

     public abstract uploadFile(response: ResponseContext): Promise<ApiResponse >;

}
