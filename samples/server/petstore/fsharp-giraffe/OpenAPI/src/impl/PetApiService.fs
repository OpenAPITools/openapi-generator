namespace OpenAPI
open OpenAPI.Model.ApiResponse
open OpenAPI.Model.Pet
open PetApiHandlerParams
open PetApiServiceInterface
open System.Collections.Generic
open System
open Giraffe

module PetApiServiceImplementation =
    
    //#region Service implementation
    type PetApiServiceImpl() = 
      interface IPetApiService with
      
        member this.AddPet ctx args =
          if true then 
            let content = "successful operation" :> obj :?> Pet // this cast is obviously wrong, and is only intended to allow generated project to compile   
            AddPetStatusCode200 { content = content }
          else
            let content = "Invalid input" 
            AddPetStatusCode405 { content = content }

        member this.DeletePet ctx args =
            let content = "Invalid pet value" 
            DeletePetStatusCode400 { content = content }

        member this.FindPetsByStatus ctx args =
          if true then 
            let content = "successful operation" :> obj :?> Pet[] // this cast is obviously wrong, and is only intended to allow generated project to compile   
            FindPetsByStatusStatusCode200 { content = content }
          else
            let content = "Invalid status value" 
            FindPetsByStatusStatusCode400 { content = content }

        member this.FindPetsByTags ctx args =
          if true then 
            let content = "successful operation" :> obj :?> Pet[] // this cast is obviously wrong, and is only intended to allow generated project to compile   
            FindPetsByTagsStatusCode200 { content = content }
          else
            let content = "Invalid tag value" 
            FindPetsByTagsStatusCode400 { content = content }

        member this.GetPetById ctx args =
          if true then 
            let content = "successful operation" :> obj :?> Pet // this cast is obviously wrong, and is only intended to allow generated project to compile   
            GetPetByIdStatusCode200 { content = content }
          else if true then 
            let content = "Invalid ID supplied" 
            GetPetByIdStatusCode400 { content = content }
          else
            let content = "Pet not found" 
            GetPetByIdStatusCode404 { content = content }

        member this.UpdatePet ctx args =
          if true then 
            let content = "successful operation" :> obj :?> Pet // this cast is obviously wrong, and is only intended to allow generated project to compile   
            UpdatePetStatusCode200 { content = content }
          else if true then 
            let content = "Invalid ID supplied" 
            UpdatePetStatusCode400 { content = content }
          else if true then 
            let content = "Pet not found" 
            UpdatePetStatusCode404 { content = content }
          else
            let content = "Validation exception" 
            UpdatePetStatusCode405 { content = content }

        member this.UpdatePetWithForm ctx args =
            let content = "Invalid input" 
            UpdatePetWithFormStatusCode405 { content = content }

        member this.UploadFile ctx args =
            let content = "successful operation" :> obj :?> ApiResponse // this cast is obviously wrong, and is only intended to allow generated project to compile   
            UploadFileStatusCode200 { content = content }

      //#endregion

    let PetApiService = PetApiServiceImpl() :> IPetApiService