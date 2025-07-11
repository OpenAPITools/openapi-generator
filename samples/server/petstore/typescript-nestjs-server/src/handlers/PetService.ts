import { Observable } from 'rxjs';

import { Injectable } from '@nestjs/common';
import { PetApi } from '../../builds/default/api';
import { ApiResponse, Pet } from '../../builds/default/models';

@Injectable()
export class PetService implements PetApi {
  addPet(pet: Pet, request: Request): Pet | Promise<Pet> | Observable<Pet> {
    console.log(JSON.stringify(pet));
    return pet;
  }

  deletePet(
    petId: number,
    apiKey: string,
    request: Request,
  ): void | Promise<void> | Observable<void> {
    throw new Error('Method not implemented.');
  }

  findPetsByStatus(
    status: Array<'available' | 'pending' | 'sold'>,
    request: Request,
  ): Array<Pet> | Promise<Array<Pet>> | Observable<Array<Pet>> {
    return [
      ...(status.includes('available') ? [{name: 'available', photoUrls: []}] : [] ),
      ...(status.includes('pending') ? [{name: 'pending', photoUrls: []}] : [] ),
      ...(status.includes('sold') ? [{name: 'sold', photoUrls: []}] : [] )
    ];
  }

  findPetsByTags(
    tags: Array<string>,
    request: Request,
  ): Array<Pet> | Promise<Array<Pet>> | Observable<Array<Pet>> {
    throw new Error('Method not implemented.');
  }

  getPetById(
    petId: number,
    request: Request,
  ): Pet | Promise<Pet> | Observable<Pet> {
    return [
      {
        name: 'MyPetA',
        photoUrls: [],
      },
      {
        name: 'MyPetB',
        photoUrls: [],
      },
    ][petId];
  }

  updatePet(pet: Pet, request: Request): Pet | Promise<Pet> | Observable<Pet> {
    throw new Error('Method not implemented.');
  }

  updatePetWithForm(
    petId: number,
    name: string,
    status: string,
    request: Request,
  ): void | Promise<void> | Observable<void> {
    throw new Error('Method not implemented.');
  }

  uploadFile(
    petId: number,
    additionalMetadata: string,
    file: Blob,
    request: Request,
  ): ApiResponse | Promise<ApiResponse> | Observable<ApiResponse> {
    throw new Error('Method not implemented.');
  }
}
