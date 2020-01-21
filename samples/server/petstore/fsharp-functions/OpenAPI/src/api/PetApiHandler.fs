namespace OpenAPI

open PetApiHandlerParams
open PetApiServiceImplementation
open Microsoft.AspNetCore.Mvc
open Microsoft.AspNetCore.Http
open Newtonsoft.Json
open Microsoft.Azure.WebJobs
open System.IO

module PetApiHandlers =

    /// <summary>
    /// 
    /// </summary>

    //#region AddPet
    /// <summary>
    /// Add a new pet to the store
    /// </summary>
   [<FunctionName("AddPet")>]
    let AddPet
        ([<HttpTrigger(Extensions.Http.AuthorizationLevel.Anonymous, "POST", Route = "/v2/pet")>]
        req:HttpRequest ) =
      
      use reader = StreamReader(req.Body)

      let mediaTypes = ["application/json";"application/xml";] // currently unused
      
      let bind (contentType:string) body  = 
        match (contentType.ToLower()) with 
        | "application/json" -> 
          body |> JsonConvert.DeserializeObject<AddPetBodyParams> 
        | _ -> failwith (sprintf "TODO - ContentType %s not currently supported" contentType)

      let bodyParams = reader.ReadToEnd() |> bind req.ContentType          
      let result = PetApiService.AddPet bodyParams
      match result with 
      | AddPetStatusCode405 resolved ->
          let content = resolved.content
          let responseContentType = "text/plain"
          ContentResult(Content = content, ContentType = responseContentType, StatusCode = System.Nullable(405)) 

    //#region DeletePet
    /// <summary>
    /// Deletes a pet
    /// </summary>
   [<FunctionName("DeletePet")>]
    let DeletePet
        ([<HttpTrigger(Extensions.Http.AuthorizationLevel.Anonymous, "DELETE", Route = "/v2/pet/{petId}")>]
        req:HttpRequest ) =
      
      let result = PetApiService.DeletePet ()
      match result with 
      | DeletePetStatusCode400 resolved ->
          let content = resolved.content
          let responseContentType = "text/plain"
          ContentResult(Content = content, ContentType = responseContentType, StatusCode = System.Nullable(400)) 

    //#region FindPetsByStatus
    /// <summary>
    /// Finds Pets by status
    /// </summary>
   [<FunctionName("FindPetsByStatus")>]
    let FindPetsByStatus
        ([<HttpTrigger(Extensions.Http.AuthorizationLevel.Anonymous, "GET", Route = "/v2/pet/findByStatus")>]
        req:HttpRequest ) =
      
      let result = PetApiService.FindPetsByStatus ()
      match result with 
      | FindPetsByStatusDefaultStatusCode resolved ->
          let content = JsonConvert.SerializeObject resolved.content
          let responseContentType = "application/json"
          ContentResult(Content = content, ContentType = responseContentType, StatusCode = System.Nullable(200)) 
      | FindPetsByStatusStatusCode400 resolved ->
          let content = resolved.content
          let responseContentType = "text/plain"
          ContentResult(Content = content, ContentType = responseContentType, StatusCode = System.Nullable(400)) 

    //#region FindPetsByTags
    /// <summary>
    /// Finds Pets by tags
    /// </summary>
   [<FunctionName("FindPetsByTags")>]
    let FindPetsByTags
        ([<HttpTrigger(Extensions.Http.AuthorizationLevel.Anonymous, "GET", Route = "/v2/pet/findByTags")>]
        req:HttpRequest ) =
      
      let result = PetApiService.FindPetsByTags ()
      match result with 
      | FindPetsByTagsDefaultStatusCode resolved ->
          let content = JsonConvert.SerializeObject resolved.content
          let responseContentType = "application/json"
          ContentResult(Content = content, ContentType = responseContentType, StatusCode = System.Nullable(200)) 
      | FindPetsByTagsStatusCode400 resolved ->
          let content = resolved.content
          let responseContentType = "text/plain"
          ContentResult(Content = content, ContentType = responseContentType, StatusCode = System.Nullable(400)) 

    //#region GetPetById
    /// <summary>
    /// Find pet by ID
    /// </summary>
   [<FunctionName("GetPetById")>]
    let GetPetById
        ([<HttpTrigger(Extensions.Http.AuthorizationLevel.Anonymous, "GET", Route = "/v2/pet/{petId}")>]
        req:HttpRequest ) =
      
      let result = PetApiService.GetPetById ()
      match result with 
      | GetPetByIdDefaultStatusCode resolved ->
          let content = JsonConvert.SerializeObject resolved.content
          let responseContentType = "application/json"
          ContentResult(Content = content, ContentType = responseContentType, StatusCode = System.Nullable(200)) 
      | GetPetByIdStatusCode400 resolved ->
          let content = resolved.content
          let responseContentType = "text/plain"
          ContentResult(Content = content, ContentType = responseContentType, StatusCode = System.Nullable(400)) 
      | GetPetByIdStatusCode404 resolved ->
          let content = resolved.content
          let responseContentType = "text/plain"
          ContentResult(Content = content, ContentType = responseContentType, StatusCode = System.Nullable(404)) 

    //#region UpdatePet
    /// <summary>
    /// Update an existing pet
    /// </summary>
   [<FunctionName("UpdatePet")>]
    let UpdatePet
        ([<HttpTrigger(Extensions.Http.AuthorizationLevel.Anonymous, "PUT", Route = "/v2/pet")>]
        req:HttpRequest ) =
      
      use reader = StreamReader(req.Body)

      let mediaTypes = ["application/json";"application/xml";] // currently unused
      
      let bind (contentType:string) body  = 
        match (contentType.ToLower()) with 
        | "application/json" -> 
          body |> JsonConvert.DeserializeObject<UpdatePetBodyParams> 
        | _ -> failwith (sprintf "TODO - ContentType %s not currently supported" contentType)

      let bodyParams = reader.ReadToEnd() |> bind req.ContentType          
      let result = PetApiService.UpdatePet bodyParams
      match result with 
      | UpdatePetStatusCode400 resolved ->
          let content = resolved.content
          let responseContentType = "text/plain"
          ContentResult(Content = content, ContentType = responseContentType, StatusCode = System.Nullable(400)) 
      | UpdatePetStatusCode404 resolved ->
          let content = resolved.content
          let responseContentType = "text/plain"
          ContentResult(Content = content, ContentType = responseContentType, StatusCode = System.Nullable(404)) 
      | UpdatePetStatusCode405 resolved ->
          let content = resolved.content
          let responseContentType = "text/plain"
          ContentResult(Content = content, ContentType = responseContentType, StatusCode = System.Nullable(405)) 

    //#region UpdatePetWithForm
    /// <summary>
    /// Updates a pet in the store with form data
    /// </summary>
   [<FunctionName("UpdatePetWithForm")>]
    let UpdatePetWithForm
        ([<HttpTrigger(Extensions.Http.AuthorizationLevel.Anonymous, "POST", Route = "/v2/pet/{petId}")>]
        req:HttpRequest ) =
      
      let result = PetApiService.UpdatePetWithForm ()
      match result with 
      | UpdatePetWithFormStatusCode405 resolved ->
          let content = resolved.content
          let responseContentType = "text/plain"
          ContentResult(Content = content, ContentType = responseContentType, StatusCode = System.Nullable(405)) 

    //#region UploadFile
    /// <summary>
    /// uploads an image
    /// </summary>
   [<FunctionName("UploadFile")>]
    let UploadFile
        ([<HttpTrigger(Extensions.Http.AuthorizationLevel.Anonymous, "POST", Route = "/v2/pet/{petId}/uploadImage")>]
        req:HttpRequest ) =
      
      let result = PetApiService.UploadFile ()
      match result with 
      | UploadFileDefaultStatusCode resolved ->
          let content = JsonConvert.SerializeObject resolved.content
          let responseContentType = "application/json"
          ContentResult(Content = content, ContentType = responseContentType, StatusCode = System.Nullable(200)) 


      

