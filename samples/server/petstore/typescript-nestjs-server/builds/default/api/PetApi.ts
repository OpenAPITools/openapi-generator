import { Injectable } from '@nestjs/common';
import { Observable } from 'rxjs';
import { ApiResponse, Pet,  } from '../models';


@Injectable()
export abstract class PetApi {

  abstract addPet(pet: Pet,  request: Request): Pet | Promise<Pet> | Observable<Pet>;


  abstract deletePet(petId: number, apiKey: string,  request: Request): void | Promise<void> | Observable<void>;


  abstract findPetsByStatus(status: Array<'available' | 'pending' | 'sold'>,  request: Request): Array<Pet> | Promise<Array<Pet>> | Observable<Array<Pet>>;


  abstract findPetsByTags(tags: Array<string>,  request: Request): Array<Pet> | Promise<Array<Pet>> | Observable<Array<Pet>>;


  abstract getPetById(petId: number,  request: Request): Pet | Promise<Pet> | Observable<Pet>;


  abstract updatePet(pet: Pet,  request: Request): Pet | Promise<Pet> | Observable<Pet>;


  abstract updatePetWithForm(petId: number, name: string, status: string,  request: Request): void | Promise<void> | Observable<void>;


  abstract uploadFile(petId: number, additionalMetadata: string, file: Blob,  request: Request): ApiResponse | Promise<ApiResponse> | Observable<ApiResponse>;

} 