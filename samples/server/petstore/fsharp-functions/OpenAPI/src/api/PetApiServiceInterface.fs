namespace OpenAPI
open PetApiHandlerParams
open System
open Microsoft.AspNetCore.Http


module PetApiServiceInterface =
    
    //#region Service interface
    type IPetApiService = 
      abstract member AddPet : AddPetBodyParams -> AddPetResult
      abstract member DeletePet : unit -> DeletePetResult
      abstract member FindPetsByStatus : unit -> FindPetsByStatusResult
      abstract member FindPetsByTags : unit -> FindPetsByTagsResult
      abstract member GetPetById : unit -> GetPetByIdResult
      abstract member UpdatePet : UpdatePetBodyParams -> UpdatePetResult
      abstract member UpdatePetWithForm : unit -> UpdatePetWithFormResult
      abstract member UploadFile : unit -> UploadFileResult
    //#endregion