namespace OpenAPI

open System.Collections.Generic
open Giraffe
open Microsoft.AspNetCore.Http
open FSharp.Control.Tasks.V2.ContextInsensitive
open PetApiHandlerParams
open PetApiServiceInterface
open PetApiServiceImplementation
open OpenAPI.Model.ApiResponse
open OpenAPI.Model.Pet

module PetApiHandler = 

    /// <summary>
    /// 
    /// </summary>

    //#region AddPet
    /// <summary>
    /// Add a new pet to the store
    /// </summary>

    let AddPet  : HttpHandler = 
      fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
          let! bodyParams = 
            ctx.BindJsonAsync<AddPetBodyParams>()
          let serviceArgs = {     bodyParams=bodyParams } : AddPetArgs
          let result = PetApiService.AddPet ctx serviceArgs
          return! (match result with 
                      | AddPetStatusCode200 resolved ->
                            setStatusCode 200 >=> json resolved.content
                      | AddPetStatusCode405 resolved ->
                            setStatusCode 405 >=> text resolved.content
          ) next ctx
        }
    //#endregion

    //#region DeletePet
    /// <summary>
    /// Deletes a pet
    /// </summary>

    let DeletePet (pathParams:DeletePetPathParams) : HttpHandler = 
      fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
          let headerParams = {
              DeletePetHeaderParams.apiKey=ctx.TryGetRequestHeader "apiKey";
          }
          let serviceArgs = { headerParams=headerParams;   pathParams=pathParams;  } : DeletePetArgs
          let result = PetApiService.DeletePet ctx serviceArgs
          return! (match result with 
                      | DeletePetStatusCode400 resolved ->
                            setStatusCode 400 >=> text resolved.content
          ) next ctx
        }
    //#endregion

    //#region FindPetsByStatus
    /// <summary>
    /// Finds Pets by status
    /// </summary>

    let FindPetsByStatus  : HttpHandler = 
      fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
          let queryParams = ctx.TryBindQueryString<FindPetsByStatusQueryParams>()
          let serviceArgs = {  queryParams=queryParams;    } : FindPetsByStatusArgs
          let result = PetApiService.FindPetsByStatus ctx serviceArgs
          return! (match result with 
                      | FindPetsByStatusStatusCode200 resolved ->
                            setStatusCode 200 >=> json resolved.content
                      | FindPetsByStatusStatusCode400 resolved ->
                            setStatusCode 400 >=> text resolved.content
          ) next ctx
        }
    //#endregion

    //#region FindPetsByTags
    /// <summary>
    /// Finds Pets by tags
    /// </summary>

    let FindPetsByTags  : HttpHandler = 
      fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
          let queryParams = ctx.TryBindQueryString<FindPetsByTagsQueryParams>()
          let serviceArgs = {  queryParams=queryParams;    } : FindPetsByTagsArgs
          let result = PetApiService.FindPetsByTags ctx serviceArgs
          return! (match result with 
                      | FindPetsByTagsStatusCode200 resolved ->
                            setStatusCode 200 >=> json resolved.content
                      | FindPetsByTagsStatusCode400 resolved ->
                            setStatusCode 400 >=> text resolved.content
          ) next ctx
        }
    //#endregion

    //#region GetPetById
    /// <summary>
    /// Find pet by ID
    /// </summary>

    let GetPetById (pathParams:GetPetByIdPathParams) : HttpHandler = 
      fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
          let serviceArgs = {    pathParams=pathParams;  } : GetPetByIdArgs
          let result = PetApiService.GetPetById ctx serviceArgs
          return! (match result with 
                      | GetPetByIdStatusCode200 resolved ->
                            setStatusCode 200 >=> json resolved.content
                      | GetPetByIdStatusCode400 resolved ->
                            setStatusCode 400 >=> text resolved.content
                      | GetPetByIdStatusCode404 resolved ->
                            setStatusCode 404 >=> text resolved.content
          ) next ctx
        }
    //#endregion

    //#region UpdatePet
    /// <summary>
    /// Update an existing pet
    /// </summary>

    let UpdatePet  : HttpHandler = 
      fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
          let! bodyParams = 
            ctx.BindJsonAsync<UpdatePetBodyParams>()
          let serviceArgs = {     bodyParams=bodyParams } : UpdatePetArgs
          let result = PetApiService.UpdatePet ctx serviceArgs
          return! (match result with 
                      | UpdatePetStatusCode200 resolved ->
                            setStatusCode 200 >=> json resolved.content
                      | UpdatePetStatusCode400 resolved ->
                            setStatusCode 400 >=> text resolved.content
                      | UpdatePetStatusCode404 resolved ->
                            setStatusCode 404 >=> text resolved.content
                      | UpdatePetStatusCode405 resolved ->
                            setStatusCode 405 >=> text resolved.content
          ) next ctx
        }
    //#endregion

    //#region UpdatePetWithForm
    /// <summary>
    /// Updates a pet in the store with form data
    /// </summary>

    let UpdatePetWithForm (pathParams:UpdatePetWithFormPathParams) : HttpHandler = 
      fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
          let! formParams = ctx.TryBindFormAsync<UpdatePetWithFormFormParams>()
          let serviceArgs = {   formParams=formParams; pathParams=pathParams;  } : UpdatePetWithFormArgs
          let result = PetApiService.UpdatePetWithForm ctx serviceArgs
          return! (match result with 
                      | UpdatePetWithFormStatusCode405 resolved ->
                            setStatusCode 405 >=> text resolved.content
          ) next ctx
        }
    //#endregion

    //#region UploadFile
    /// <summary>
    /// uploads an image
    /// </summary>

    let UploadFile (pathParams:UploadFilePathParams) : HttpHandler = 
      fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
          let! formParams = ctx.TryBindFormAsync<UploadFileFormParams>()
          let serviceArgs = {   formParams=formParams; pathParams=pathParams;  } : UploadFileArgs
          let result = PetApiService.UploadFile ctx serviceArgs
          return! (match result with 
                      | UploadFileStatusCode200 resolved ->
                            setStatusCode 200 >=> json resolved.content
          ) next ctx
        }
    //#endregion


    
