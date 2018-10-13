import { BaseAPIRequestFactory } from './baseapi';
import { RequestContext, ResponseContext } from '../http/http';
import { ApiResponse } from '../models/ApiResponse';
import { Pet } from '../models/Pet';
export declare class PetApiRequestFactory extends BaseAPIRequestFactory {
    addPet(pet: Pet, options?: any): RequestContext;
    deletePet(petId: number, apiKey?: string, options?: any): RequestContext;
    findPetsByStatus(status: Array<'available' | 'pending' | 'sold'>, options?: any): RequestContext;
    findPetsByTags(tags: Array<string>, options?: any): RequestContext;
    getPetById(petId: number, options?: any): RequestContext;
    updatePet(pet: Pet, options?: any): RequestContext;
    updatePetWithForm(petId: number, name?: string, status?: string, options?: any): RequestContext;
    uploadFile(petId: number, additionalMetadata?: string, file?: any, options?: any): RequestContext;
}
export declare class PetApiResponseProcessor {
    addPet(response: ResponseContext): void;
    deletePet(response: ResponseContext): void;
    findPetsByStatus(response: ResponseContext): Array<Pet>;
    findPetsByTags(response: ResponseContext): Array<Pet>;
    getPetById(response: ResponseContext): Pet;
    updatePet(response: ResponseContext): void;
    updatePetWithForm(response: ResponseContext): void;
    uploadFile(response: ResponseContext): ApiResponse;
}
