namespace OpenAPI
open PetApiHandlerParams
open System
open Giraffe
open Microsoft.AspNetCore.Http


module PetApiServiceInterface =
    
    //#region Service interface
    type IPetApiService = 
      abstract member AddPet:HttpContext -> AddPetArgs->AddPetResult
      abstract member DeletePet:HttpContext -> DeletePetArgs->DeletePetResult
      abstract member FindPetsByStatus:HttpContext -> FindPetsByStatusArgs->FindPetsByStatusResult
      abstract member FindPetsByTags:HttpContext -> FindPetsByTagsArgs->FindPetsByTagsResult
      abstract member GetPetById:HttpContext -> GetPetByIdArgs->GetPetByIdResult
      abstract member UpdatePet:HttpContext -> UpdatePetArgs->UpdatePetResult
      abstract member UpdatePetWithForm:HttpContext -> UpdatePetWithFormArgs->UpdatePetWithFormResult
      abstract member UploadFile:HttpContext -> UploadFileArgs->UploadFileResult
    //#endregion