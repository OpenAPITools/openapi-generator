import { Body, Controller, DefaultValuePipe, Delete, Get, Post, Put, Param, ParseIntPipe, ParseFloatPipe, Query, Req } from '@nestjs/common';
import { Observable } from 'rxjs';
import { Cookies, Headers } from '../decorators';
import { PetApi } from '../api';
import { ApiResponse, Pet,  } from '../models';

@Controller()
export class PetApiController {
  constructor(private readonly petApi: PetApi) {}

  @Post('/pet')
  addPet(@Body() pet: Pet, @Req() request: Request): Pet | Promise<Pet> | Observable<Pet> {
    return this.petApi.addPet(pet, request);
  }

  @Delete('/pet/:petId')
  deletePet(@Param('petId') petId: number, @Headers('api_key') apiKey: string | undefined, @Req() request: Request): void | Promise<void> | Observable<void> {
    return this.petApi.deletePet(petId, apiKey, request);
  }

  @Get('/pet/findByStatus')
  findPetsByStatus(@Query('status') status: Array<'available' | 'pending' | 'sold'>, @Req() request: Request): Array<Pet> | Promise<Array<Pet>> | Observable<Array<Pet>> {
    return this.petApi.findPetsByStatus(status, request);
  }

  @Get('/pet/findByTags')
  findPetsByTags(@Query('tags') tags: Array<string>, @Req() request: Request): Array<Pet> | Promise<Array<Pet>> | Observable<Array<Pet>> {
    return this.petApi.findPetsByTags(tags, request);
  }

  @Get('/pet/:petId')
  getPetById(@Param('petId') petId: number, @Req() request: Request): Pet | Promise<Pet> | Observable<Pet> {
    return this.petApi.getPetById(petId, request);
  }

  @Put('/pet')
  updatePet(@Body() pet: Pet, @Req() request: Request): Pet | Promise<Pet> | Observable<Pet> {
    return this.petApi.updatePet(pet, request);
  }

  @Post('/pet/:petId')
  updatePetWithForm(@Param('petId') petId: number, name: string | undefined, status: string | undefined, @Req() request: Request): void | Promise<void> | Observable<void> {
    return this.petApi.updatePetWithForm(petId, name, status, request);
  }

  @Post('/pet/:petId/uploadImage')
  uploadFile(@Param('petId') petId: number, additionalMetadata: string | undefined, file: Blob | undefined, @Req() request: Request): ApiResponse | Promise<ApiResponse> | Observable<ApiResponse> {
    return this.petApi.uploadFile(petId, additionalMetadata, file, request);
  }

} 