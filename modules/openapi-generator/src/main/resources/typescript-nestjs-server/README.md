#  OpenApi Generator _typescript-nestjs-server_

Usage: The generated output is intended to be its own module, that can be imported into your NestJS App Module. You do not need to change generated files, just import the module and implement the API

Example usage (with the openapi sample `petstore.yaml`):

1. Invoke openapi-generator
   ```
   openapi-generator-cli.jar generate -i petstore.yaml -g typescript-nestjs-server -o api-module/
   ```
2. implement the contracts from `api-module/api`  
   `handlers/PetService.ts`:
   ```typescript
   import { Pet, ApiResponse } from "models";
   import { Observable } from "rxjs";
   import { PetApi } from "../api";
   import { Inject, Injectable } from "@nestjs/common";
   
   @Injectable()
   export class PetService implements PetApi {
     addPet(pet: Pet, request: Request): Pet | Promise<Pet> | Observable<Pet> {
       throw new Error("Method not implemented.");
     }
   
     deletePet(petId: number, apiKey: string, request: Request): void | Promise<void> | Observable<void> {
       throw new Error("Method not implemented.");
     }
   
   ...
   ```
3. Import the generated `ApiModule` with `ApiModule.forRoot` and provide a instance of `ApiImplementations` with a reference to your implementation  
   `app.module.ts`
   ```typescript
   import { Module } from "@nestjs/common";
   import { ApiModule, ApiImplementations } from "api-module";
   import { PetService } from "./handlers/PetService";
   import { UserService } from "./handlers/UserService";
   import { StoreService } from "./handlers/StoreService";
   
   const apiImplementations: ApiImplementations = {
     petApi: PetService,
     userApi: UserService,
     storeApi: StoreService,
   }
   
   @Module({
     imports: [
       ApiModule.forRoot(apiImplementations),
     ],
     controllers: [],
     providers: [],
   })
   export class AppModule {}
   ```

You now can regenerate the API module as often as you want without overriding your implementation.