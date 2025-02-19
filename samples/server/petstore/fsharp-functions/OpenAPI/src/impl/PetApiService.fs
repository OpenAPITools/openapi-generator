namespace OpenAPI
open OpenAPI.Model.ApiResponse
open OpenAPI.Model.Pet
open PetApiHandlerParams
open PetApiServiceInterface
open System.Collections.Generic
open System

module PetApiServiceImplementation =
    
    //#region Service implementation
    type PetApiServiceImpl() = 
      interface IPetApiService with
      
        member this.AddPet (parameters:AddPetBodyParams) =
            let content = "Invalid input" 
            AddPetStatusCode405 { content = content }

        member this.DeletePet () =
            let content = "Invalid pet value" 
            DeletePetStatusCode400 { content = content }

        member this.FindPetsByStatus () =
          if true then 
            let content = "successful operation" :> obj :?> Pet[] // this cast is obviously wrong, and is only intended to allow generated project to compile   
            FindPetsByStatusDefaultStatusCode { content = content }
          else
            let content = "Invalid status value" 
            FindPetsByStatusStatusCode400 { content = content }

        member this.FindPetsByTags () =
          if true then 
            let content = "successful operation" :> obj :?> Pet[] // this cast is obviously wrong, and is only intended to allow generated project to compile   
            FindPetsByTagsDefaultStatusCode { content = content }
          else
            let content = "Invalid tag value" 
            FindPetsByTagsStatusCode400 { content = content }

        member this.GetPetById () =
          if true then 
            let content = "successful operation" :> obj :?> Pet // this cast is obviously wrong, and is only intended to allow generated project to compile   
            GetPetByIdDefaultStatusCode { content = content }
          else if true then 
            let content = "Invalid ID supplied" 
            GetPetByIdStatusCode400 { content = content }
          else
            let content = "Pet not found" 
            GetPetByIdStatusCode404 { content = content }

        member this.UpdatePet (parameters:UpdatePetBodyParams) =
          if true then 
            let content = "Invalid ID supplied" 
            UpdatePetStatusCode400 { content = content }
          else if true then 
            let content = "Pet not found" 
            UpdatePetStatusCode404 { content = content }
          else
            let content = "Validation exception" 
            UpdatePetStatusCode405 { content = content }

        member this.UpdatePetWithForm () =
            let content = "Invalid input" 
            UpdatePetWithFormStatusCode405 { content = content }

        member this.UploadFile () =
            let content = "successful operation" :> obj :?> ApiResponse // this cast is obviously wrong, and is only intended to allow generated project to compile   
            UploadFileDefaultStatusCode { content = content }

      //#endregion

    let PetApiService = PetApiServiceImpl() :> IPetApiService