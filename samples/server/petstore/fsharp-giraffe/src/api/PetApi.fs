namespace OpenAPI

open Giraffe
open Microsoft.AspNetCore.Http
open FSharp.Control.Tasks.V2.ContextInsensitive

module PetApiHandler = 

    /// <summary>
    /// 
    /// </summary>

    //#region AddPet
    /// <summary>
    /// Add a new pet to the store
    /// </summary>

    //#region Body parameters
    [<CLIMutable>]
    type AddPetBodyParams = {
      body : Pet
    }
    //#endregion
  
    let AddPet  : HttpHandler = 
      fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
          let! body = 
            ctx.BindJsonAsync<AddPetBodyParams>()
          return! null
        }
    //#endregion

    //#region DeletePet
    /// <summary>
    /// Deletes a pet
    /// </summary>

    //#region Path parameters
    [<CLIMutable>]
    type DeletePetPathParams = {
      petId : int64
    }
    //#endregion

    //#region Header parameters
    [<CLIMutable>]
    type DeletePetHeaderParams = {
      apiKey : Option<string>
    }
    //#endregion
  
    let DeletePet (args:DeletePetPathParams) : HttpHandler = 
      fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
          return! null
        }
    //#endregion

    //#region FindPetsByStatus
    /// <summary>
    /// Finds Pets by status
    /// </summary>

    //#region Query parameters
    [<CLIMutable>]
    type FindPetsByStatusQueryParams = {
      status : string[]
    }
    //#endregion
  
    let FindPetsByStatus  : HttpHandler = 
      fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
          let! queryParams = ctx.TryBindQueryString<FindPetsByStatusQueryParams>
          return! null
        }
    //#endregion

    //#region FindPetsByTags
    /// <summary>
    /// Finds Pets by tags
    /// </summary>

    //#region Query parameters
    [<CLIMutable>]
    type FindPetsByTagsQueryParams = {
      tags : string[]
    }
    //#endregion
  
    let FindPetsByTags  : HttpHandler = 
      fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
          let! queryParams = ctx.TryBindQueryString<FindPetsByTagsQueryParams>
          return! null
        }
    //#endregion

    //#region GetPetById
    /// <summary>
    /// Find pet by ID
    /// </summary>

    //#region Path parameters
    [<CLIMutable>]
    type GetPetByIdPathParams = {
      petId : int64
    }
    //#endregion
  
    let GetPetById (args:GetPetByIdPathParams) : HttpHandler = 
      fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
          return! null
        }
    //#endregion

    //#region UpdatePet
    /// <summary>
    /// Update an existing pet
    /// </summary>

    //#region Body parameters
    [<CLIMutable>]
    type UpdatePetBodyParams = {
      body : Pet
    }
    //#endregion
  
    let UpdatePet  : HttpHandler = 
      fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
          let! body = 
            ctx.BindJsonAsync<UpdatePetBodyParams>()
          return! null
        }
    //#endregion

    //#region UpdatePetWithForm
    /// <summary>
    /// Updates a pet in the store with form data
    /// </summary>

    //#region Path parameters
    [<CLIMutable>]
    type UpdatePetWithFormPathParams = {
      petId : int64
    }
    //#endregion

    //#region Header parameters
    [<CLIMutable>]
    type UpdatePetWithFormHeaderParams = {
      name : Option<string>
    //#endregion

    //#region Header parameters
      status : Option<string>
    }
    //#endregion
  
    let UpdatePetWithForm (args:UpdatePetWithFormPathParams) : HttpHandler = 
      fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
          let! name = 
            ctx.TryBindFormAsync<UpdatePetWithFormFormParams>()
          let! status = 
            ctx.TryBindFormAsync<UpdatePetWithFormFormParams>()
          return! null
        }
    //#endregion

    //#region UploadFile
    /// <summary>
    /// uploads an image
    /// </summary>

    //#region Path parameters
    [<CLIMutable>]
    type UploadFilePathParams = {
      petId : int64
    }
    //#endregion

    //#region Header parameters
    [<CLIMutable>]
    type UploadFileHeaderParams = {
      additionalMetadata : Option<string>
    //#endregion

    //#region Header parameters
      file : Option<System.IO.Stream>
    }
    //#endregion
  
    let UploadFile (args:UploadFilePathParams) : HttpHandler = 
      fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
          let! additionalMetadata = 
            ctx.TryBindFormAsync<UploadFileFormParams>()
          let! file = 
            ctx.TryBindFormAsync<UploadFileFormParams>()
          return! null
        }
    //#endregion

